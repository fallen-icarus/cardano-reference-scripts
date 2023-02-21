{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Test.Common where

import Data.Void (Void)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Data.List (foldl')
import Prelude as Haskell (Semigroup (..), String)
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidator)

import CardanoReferenceScripts

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Maybe ScriptHash -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum maybeRef val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum maybeRef val

data BeaconRedeemer' = MintBeacon' ScriptHash | BurnBeacon'
  deriving (Generic,ToJSON,FromJSON)

convert2BeaconRedeemer :: BeaconRedeemer' -> BeaconRedeemer
convert2BeaconRedeemer (MintBeacon' sh) = MintBeacon sh
convert2BeaconRedeemer BurnBeacon' = BurnBeacon

-------------------------------------------------
-- Trace Configs
-------------------------------------------------
data CreateSharedReferenceScript = CreateSharedReferenceScript
  { beaconsMinted :: [(TokenName,Integer)]
  , mintRedeemer :: BeaconRedeemer'
  , createRefVaultAddress :: Address
  , createRefScript :: Maybe ScriptHash
  , createRefScriptUTxO :: (Maybe CurrencySymbol,Value)
  , createOtherAddress :: Address
  , createOtherScriptUTxOs :: [(Maybe CurrencySymbol,Value)]
  , createAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON)

data CloseSharedReferenceScript = CloseSharedReferenceScript
  { beaconsBurned :: [(TokenName,Integer)]
  , burnRedeemer :: BeaconRedeemer'
  , closeAddress :: Address
  , closeUTxOs :: [(CurrencySymbol,Value)]
  , newUTxOsAtAddress :: [(Maybe CurrencySymbol,Value)]
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "create-shared-reference-script" CreateSharedReferenceScript
  .\/ Endpoint "close-shared-reference-script" CloseSharedReferenceScript

-------------------------------------------------
-- Trace Models
-------------------------------------------------
createSharedReferenceScript :: CreateSharedReferenceScript -> Contract () TraceSchema Text ()
createSharedReferenceScript CreateSharedReferenceScript{..} = do
  let beaconPolicyHash = mintingPolicyHash beaconPolicy
      beaconRedeemer = toRedeemer $ convert2BeaconRedeemer mintRedeemer

      toDatum'
        | createAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript referenceVaultValidator
             <> plutusV2OtherScript alwaysSucceedValidator
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          beaconsMinted
        )
        -- | Must store reference script
        <> mustPayToAddressWith 
            createRefVaultAddress 
            (fmap toDatum' $ fst createRefScriptUTxO) 
            createRefScript 
            (snd createRefScriptUTxO)
        -- | Add other address utxos
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith createOtherAddress (fmap toDatum' d) Nothing v)
              mempty
              createOtherScriptUTxOs
           )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Created shared reference script"

closeSharedReferenceScript :: CloseSharedReferenceScript -> Contract () TraceSchema Text ()
closeSharedReferenceScript CloseSharedReferenceScript{..} = do
  vaultUtxos <- utxosAt closeAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconPolicyHash = mintingPolicyHash beaconPolicy
      beaconRedeemer = toRedeemer $ convert2BeaconRedeemer burnRedeemer

      toDatum' = TxOutDatumInline . toDatum

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript referenceVaultValidator
             <> Constraints.unspentOutputs vaultUtxos

      tx' =
        -- | Must burn beacon
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          beaconsBurned
        )
        -- | Must spend all utxos to be closed.
        <> (foldl' 
              (\a (d,v) -> a 
                        <> mustSpendScriptOutputWithMatchingDatumAndValue 
                              referenceVaultHash 
                              (== toDatum d) 
                              (==v) 
                              beaconRedeemer) 
              mempty 
              closeUTxOs
           )
        -- | Must be signed by stake pubkey (same as payment for this model)
        <> mustBeSignedBy userPubKeyHash
        -- | Add other address utxos
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith closeAddress (fmap toDatum' d) Nothing v)
              mempty
              newUTxOsAtAddress
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Closed a shared reference script"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createSharedReferenceScript' = endpoint @"create-shared-reference-script" createSharedReferenceScript
    closeSharedReferenceScript' = endpoint @"close-shared-reference-script" closeSharedReferenceScript
    choices = 
      [ createSharedReferenceScript'
      , closeSharedReferenceScript'
      ]