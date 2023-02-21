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

module Test.CreateSharedRef
(
  tests,
  testTrace
) where

import Prelude (IO)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)

import Test.Common

import CardanoReferenceScripts

-------------------------------------------------
-- Creation Scenarios
-------------------------------------------------
properlyShareRefScript :: EmulatorTrace ()
properlyShareRefScript = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

storeInPubKeyAddress :: EmulatorTrace ()
storeInPubKeyAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (PubKeyCredential $ unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

storeInOtherScriptAddress :: EmulatorTrace ()
storeInOtherScriptAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential alwaysSucceedValidatorHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

beaconStoredInPubKeyAddress :: EmulatorTrace ()
beaconStoredInPubKeyAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

beaconStoredInOtherScriptAddress :: EmulatorTrace ()
beaconStoredInOtherScriptAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

      otherScriptAddr = Address
        (ScriptCredential alwaysSucceedValidatorHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000
          )
      , createOtherAddress = otherScriptAddr
      , createOtherScriptUTxOs = 
          [ ( Just beaconSymbol
            , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      , createAsInline = True
      }

beaconStoredInDifferentVaultUTxO :: EmulatorTrace ()
beaconStoredInDifferentVaultUTxO = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = 
          [ ( Just beaconSymbol
            , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      , createAsInline = True
      }

beaconStoredWithDifferentReferenceScript :: EmulatorTrace ()
beaconStoredWithDifferentReferenceScript = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just $ (\(ValidatorHash s) -> ScriptHash s) alwaysSucceedValidatorHash
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

mintMoreThanJustThatBeacon :: EmulatorTrace ()
mintMoreThanJustThatBeacon = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = 
          [ (scriptHashAsToken refScript,1)
          , (scriptHashAsToken $ (\(ValidatorHash s) -> ScriptHash s) alwaysSucceedValidatorHash, 1)
          ]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

mintManyOfThatBeacon :: EmulatorTrace ()
mintManyOfThatBeacon = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,3)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

mintWithBurnRedeemer :: EmulatorTrace ()
mintWithBurnRedeemer = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = BurnBeacon'
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

beaconDatumHasDifferentPolicyId :: EmulatorTrace ()
beaconDatumHasDifferentPolicyId = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just adaSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

datumNotInline :: EmulatorTrace ()
datumNotInline = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = False
      }

vaultHasNoStakingCredential :: EmulatorTrace ()
vaultHasNoStakingCredential = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        Nothing

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

beaconNotStoredWithReferenceScript :: EmulatorTrace ()
beaconNotStoredWithReferenceScript = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) referenceVaultHash
      refVaultAddr = Address
        (ScriptCredential referenceVaultHash)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Nothing
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  testGroup "Creating Shared Reference Scripts"
    [ checkPredicate "Fail if reference script and beacon stored in pubkey address"
        (Test.not assertNoFailedTransactions) storeInPubKeyAddress
    , checkPredicate "Fail if reference script and beacon stored in other script address"
        (Test.not assertNoFailedTransactions) storeInOtherScriptAddress
    , checkPredicate "Fail if beacon stored in pubkey address but reference script in vault"
        (Test.not assertNoFailedTransactions) beaconStoredInPubKeyAddress
    , checkPredicate "Fail if beacon stored in other script address but reference script in vault"
        (Test.not assertNoFailedTransactions) beaconStoredInOtherScriptAddress
    , checkPredicate "Fail if beacon stored in separate vault utxo than reference script"
        (Test.not assertNoFailedTransactions) beaconStoredInDifferentVaultUTxO
    , checkPredicate "Fail if beacon stored with different reference script"
        (Test.not assertNoFailedTransactions) beaconStoredWithDifferentReferenceScript
    , checkPredicate "Fail if more than just that beacon minted"
        (Test.not assertNoFailedTransactions) mintMoreThanJustThatBeacon
    , checkPredicate "Fail if many of that beacon minted"
        (Test.not assertNoFailedTransactions) mintManyOfThatBeacon
    , checkPredicate "Fail if burn redeemer used to mint"
        (Test.not assertNoFailedTransactions) mintWithBurnRedeemer
    , checkPredicate "Fail if datum is not beacon policy id"
        (Test.not assertNoFailedTransactions) beaconDatumHasDifferentPolicyId
    , checkPredicate "Fail if datum is not inline"
        (Test.not assertNoFailedTransactions) datumNotInline
    , checkPredicate "Fail if beacon minted to vault address without a staking credential"
        (Test.not assertNoFailedTransactions) vaultHasNoStakingCredential
    , checkPredicate "Fail if beacon not stored with a reference script"
        (Test.not assertNoFailedTransactions) beaconNotStoredWithReferenceScript
    , checkPredicate "Successfully create shared reference script"
        assertNoFailedTransactions properlyShareRefScript
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO beaconNotStoredWithReferenceScript