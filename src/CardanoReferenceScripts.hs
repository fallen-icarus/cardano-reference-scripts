{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}

module CardanoReferenceScripts
(
  BeaconRedeemer(..),
  ScriptHash,
  CurrencySymbol,
  TokenName,
  readScriptHash,
  scriptHashAsToken,

  referenceVaultValidator,
  referenceVaultScript,
  referenceVaultHash,

  beaconPolicy,
  beaconScript,
  beaconSymbol,

  writeData,
  writeScript
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath) 
import qualified Prelude as Haskell
import Data.String (fromString)

import Cardano.Api hiding (Address,Script,Value,TxOut,ScriptHash)
import Cardano.Api.Shelley (PlutusScript (..))
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api as PlutusApi
import qualified PlutusTx
import PlutusTx.Prelude
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Ledger.Bytes (fromHex)
import qualified Plutonomy
import Ledger.Value (valueOf,flattenValue)
import PlutusTx.Numeric as Num
import PlutusPrelude (foldl')
import qualified PlutusTx.AssocMap as Map

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Parse ScriptHash from user supplied String
readScriptHash :: Haskell.String -> Either Haskell.String ScriptHash
readScriptHash s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ ScriptHash bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

{-# INLINABLE scriptHashAsToken #-}
scriptHashAsToken :: ScriptHash -> TokenName
scriptHashAsToken (ScriptHash s) = TokenName s

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput (ScriptContext info (Spending ref)) = getScriptInput (txInfoInputs info) ref
ownInput _ = traceError "script input error"

{-# INLINABLE getScriptInput #-}
getScriptInput :: [TxInInfo] -> TxOutRef -> TxOut
getScriptInput [] _ = traceError "script input error"
getScriptInput ((TxInInfo iRef ot) : tl) ref
  | iRef == ref = ot
  | otherwise = getScriptInput tl ref

{-# INLINABLE signed #-}
signed :: [PubKeyHash] -> PubKeyHash -> Bool
signed [] _ = False
signed (k:ks) k'
  | k == k' = True
  | otherwise = signed ks k'

-------------------------------------------------
-- App Settings
-------------------------------------------------
-- | Beacon Redeemer
data BeaconRedeemer
  -- | To mint a reference script beacon, the minted token must use the hash of the reference script
  -- and the beacon must be stored in the same utxo as the reference script.
  = MintBeacon ScriptHash
  -- | Burning is always allowed as long as this redeemer is only used to burn.
  | BurnBeacon

PlutusTx.unstableMakeIsData ''BeaconRedeemer

-- | This is useful for testing without risk of accidentally locking production beacons on chain.
type AppName = BuiltinString

-------------------------------------------------
-- On-Chain Reference Vault
-------------------------------------------------
-- | The reference vault is used to hold the reference script and force burning whenever
-- the reference script utxo is consumed.
--
-- Any redeemer can be used since there is only one possible action:
-- burn the beacon and remove the reference script.
mkReferenceVault :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
mkReferenceVault beaconSym _ ctx@ScriptContext{scriptContextTxInfo = info} =
    case utxoInfo of
      -- | This option is to prevent accidental locking. Beacons can never be minted
      -- to this kind of address so this is fine.
      (Nothing,_) -> True

      -- | If there is no reference script, that means there is no beacon attached to this
      -- utxo. This utxo can be spent as long as the staking credential approves.
      (Just stakeCred, Nothing) -> stakingCredApproves stakeCred

      -- | Any beacons in inputs mut be burned and the staking credential must signal approval.
      (Just stakeCred, Just sh) -> stakingCredApproves stakeCred && beaconsBurned sh
  where
    -- | Get the necessary info from the utxo currently being validated.
    utxoInfo :: (Maybe StakingCredential,Maybe ScriptHash)
    utxoInfo = 
      let TxOut{ txOutAddress = Address{addressStakingCredential=maybeStakeCred}
               , txOutReferenceScript = maybeRef
               } = ownInput ctx
      in (maybeStakeCred,maybeRef)

    stakingCredApproves :: StakingCredential -> Bool
    stakingCredApproves stakeCred@(StakingHash cred) = case cred of
      PubKeyCredential pkh -> traceIfFalse "stake key did not sign" $
        signed (txInfoSignatories info) pkh
      ScriptCredential _ -> traceIfFalse "stake script not executed" $ 
        isJust $ Map.lookup stakeCred $ txInfoWdrl info
    stakingCredApproves _ = traceError "Wrong kind of staking credential."

    -- | Checks all inputs for double satisfaction problem.
    beaconInput :: TokenName -> Integer
    beaconInput refName =
      let inputs = txInfoInputs info
          foo acc TxInInfo{txInInfoResolved=TxOut{txOutValue=iVal}} =
            acc + valueOf iVal beaconSym refName
      in foldl' foo 0 inputs

    beaconsBurned :: ScriptHash -> Bool
    beaconsBurned sh = 
      let refName = scriptHashAsToken sh 
      in traceIfFalse "Beacons not burned" $
           Num.negate (beaconInput refName) == valueOf (txInfoMint info) beaconSym refName

data ReferenceVault
instance ValidatorTypes ReferenceVault where
  type instance RedeemerType ReferenceVault = BuiltinData
  type instance DatumType ReferenceVault = CurrencySymbol

referenceVaultValidator :: Validator
referenceVaultValidator = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @ReferenceVault
    ($$(PlutusTx.compile [|| mkReferenceVault ||]))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator

referenceVaultScript :: Script
referenceVaultScript = unValidatorScript referenceVaultValidator

referenceVaultHash :: ValidatorHash
referenceVaultHash = Scripts.validatorHash referenceVaultValidator

-------------------------------------------------
-- On-Chain Reference Beacon
-------------------------------------------------
mkBeaconPolicy :: AppName -> ValidatorHash  -- ^ Extra parameters
               -> BeaconRedeemer -> ScriptContext -> Bool
mkBeaconPolicy appName refVaultHash r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
    MintBeacon sh ->
      -- | Must mint one beacon with correct token name.
      mintCheck &&
      -- | Must be minted to proper utxo.
      destinationCheck sh
    BurnBeacon -> 
      -- | Can only be used to burn.
      mintCheck
  where
    beaconSym :: CurrencySymbol
    beaconSym = ownCurrencySymbol ctx

    -- | Returns only the beacons minted/burned
    beaconMint :: [(CurrencySymbol,TokenName,Integer)]
    beaconMint = case Map.lookup beaconSym $ getValue $ txInfoMint info of
      Nothing -> traceError "MintError"
      Just bs -> flattenValue $ Value $ Map.insert beaconSym bs Map.empty -- ^ a Value with only beacons

    mintCheck :: Bool
    mintCheck = case (r, beaconMint) of
      (MintBeacon sh, [(_,tn,n)]) -> 
        let name = scriptHashAsToken sh
        in if n < 1 then traceError "Must mint with this redeemer"
           else
             traceIfFalse "Can only mint the beacon with this script hash as token name" (tn == name) &&
             traceIfFalse "One, and only one, beacon must be minted with this redeemer." (n == 1)
      (MintBeacon _, _) -> traceError "Can only mint the beacon with this script hash as token name"
      (BurnBeacon, xs) ->
        traceIfFalse "Beacons can only be burned with this redeemer" (all (\(_,_,n) -> n < 0) xs)

    parseDatum :: OutputDatum -> CurrencySymbol
    parseDatum d = case d of
      (OutputDatum (Datum d')) -> case fromBuiltinData d' of
        Nothing -> traceError "Invalid datum stored with beacon."
        Just p -> p
      _ -> traceError "All reference vault datums must be inline datums."
    
    validDatum :: CurrencySymbol -> Bool
    validDatum currSym
      | currSym /= beaconSym = traceError "Datum not beacon policy id"
      | otherwise = True

    -- | A helper function for destinationCheck to make the code easier to reason about.
    -- This uses the appName in the error message.
    validDestination :: ScriptHash -> ValidatorHash -> ScriptHash -> Bool
    validDestination beaconRefScript spendVh refScript
      | spendVh /= refVaultHash = traceError ("Beacon not minted to the proper " <> appName <> " address")
      | beaconRefScript /= refScript = 
          traceError "Beacon not stored with the reference script for that script hash"
      | otherwise = True

    -- | Check if the beacon is going ot a valid address with a valid datum and is stored with
    -- the proper reference script.
    destinationCheck :: ScriptHash -> Bool
    destinationCheck beaconRefScript =
      let outputs = txInfoOutputs info
          name = scriptHashAsToken beaconRefScript
          foo acc TxOut{txOutDatum=d
                       ,txOutValue=oVal
                       ,txOutReferenceScript=maybeRef
                       ,txOutAddress=Address{addressCredential=addrCred
                                            ,addressStakingCredential=maybeStakeCred
                                            }
                       } =
            let datum = parseDatum d
            in if valueOf oVal beaconSym name == 1
               then case (addrCred,maybeStakeCred,maybeRef) of
                (ScriptCredential vh, Just (StakingHash _),Just ss) ->
                  -- | validDestination and validDatum will both fail with traceError unless True.
                  acc && validDestination beaconRefScript vh ss && validDatum datum
                (ScriptCredential _, _, Just _) -> 
                  traceError "Beacon not minted to a script address with a proper staking credential"
                (ScriptCredential _, _ , Nothing) ->
                  traceError "Beacon not stored with a reference script"
                (PubKeyCredential _, _, _) ->
                  traceError ("Beacon not minted to the proper " <> appName <> " address")
               else acc
      in foldl' foo True outputs

beaconPolicy' :: AppName -> ValidatorHash -> MintingPolicy
beaconPolicy' appName refVaultHash = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
   ($$(PlutusTx.compile [|| wrap ||])
     `PlutusTx.applyCode` PlutusTx.liftCode appName
     `PlutusTx.applyCode` PlutusTx.liftCode refVaultHash)
  where
    wrap x y = mkUntypedMintingPolicy $ mkBeaconPolicy x y

beaconPolicy :: MintingPolicy
beaconPolicy = beaconPolicy' "testing" referenceVaultHash

beaconScript :: Script
beaconScript = unMintingPolicyScript beaconPolicy

beaconSymbol :: CurrencySymbol
beaconSymbol = scriptCurrencySymbol beaconPolicy

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file 
               . encode 
               . scriptDataToJson ScriptDataJsonDetailedSchema 
               . dataToScriptData 
               . PlutusTx.toData

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                         $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData = writeJSON