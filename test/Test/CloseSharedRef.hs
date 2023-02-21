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

module Test.CloseSharedRef
(
  tests,
  testTrace
) where

import Prelude (IO)
import PlutusTx.Prelude
import Control.Monad (void)
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
-- Close Scenarios
-------------------------------------------------
properlyCloseSharedRefScript :: EmulatorTrace ()
properlyCloseSharedRefScript = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) alwaysSucceedValidatorHash
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

  void $ waitUntilSlot 2

  callEndpoint @"close-shared-reference-script" h1 $
    CloseSharedReferenceScript
      { beaconsBurned = [(scriptHashAsToken refScript,-1)]
      , burnRedeemer = BurnBeacon'
      , closeAddress = refVaultAddr
      , closeUTxOs =
          [ ( beaconSymbol
            , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      , newUTxOsAtAddress = []
      }

closeAsNonOwner :: EmulatorTrace ()
closeAsNonOwner = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) alwaysSucceedValidatorHash
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

  void $ waitUntilSlot 2

  callEndpoint @"close-shared-reference-script" h2 $
    CloseSharedReferenceScript
      { beaconsBurned = [(scriptHashAsToken refScript,-1)]
      , burnRedeemer = BurnBeacon'
      , closeAddress = refVaultAddr
      , closeUTxOs =
          [ ( beaconSymbol
            , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      , newUTxOsAtAddress = []
      }

closeAndWithdrawBeacon :: EmulatorTrace ()
closeAndWithdrawBeacon = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) alwaysSucceedValidatorHash
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

  void $ waitUntilSlot 2

  callEndpoint @"close-shared-reference-script" h1 $
    CloseSharedReferenceScript
      { beaconsBurned = []
      , burnRedeemer = BurnBeacon'
      , closeAddress = refVaultAddr
      , closeUTxOs =
          [ ( beaconSymbol
            , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      , newUTxOsAtAddress = []
      }

closeButLeaveBeacon :: EmulatorTrace ()
closeButLeaveBeacon = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) alwaysSucceedValidatorHash
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

  void $ waitUntilSlot 2

  callEndpoint @"close-shared-reference-script" h1 $
    CloseSharedReferenceScript
      { beaconsBurned = []
      , burnRedeemer = BurnBeacon'
      , closeAddress = refVaultAddr
      , closeUTxOs =
          [ ( beaconSymbol
            , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      , newUTxOsAtAddress = 
          [ ( Just beaconSymbol
            , lovelaceValueOf 2_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      }

closeManyRefScripts :: EmulatorTrace ()
closeManyRefScripts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) alwaysSucceedValidatorHash
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

  void $ waitUntilSlot 2

  callEndpoint @"create-shared-reference-script" h1 $
    CreateSharedReferenceScript
      { beaconsMinted = [(scriptHashAsToken refScript,1)]
      , mintRedeemer = MintBeacon' refScript
      , createRefVaultAddress = refVaultAddr
      , createRefScript = Just refScript
      , createRefScriptUTxO = 
          ( Just beaconSymbol
          , lovelaceValueOf 22_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
          )
      , createOtherAddress = refVaultAddr
      , createOtherScriptUTxOs = []
      , createAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"close-shared-reference-script" h1 $
    CloseSharedReferenceScript
      { beaconsBurned = [(scriptHashAsToken refScript,-2)]
      , burnRedeemer = BurnBeacon'
      , closeAddress = refVaultAddr
      , closeUTxOs =
          [ ( beaconSymbol
            , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          , ( beaconSymbol
            , lovelaceValueOf 22_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      , newUTxOsAtAddress = []
      }

closeWithMintRedeemer :: EmulatorTrace ()
closeWithMintRedeemer = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refScript = (\(ValidatorHash s) -> ScriptHash s) alwaysSucceedValidatorHash
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

  void $ waitUntilSlot 2

  callEndpoint @"close-shared-reference-script" h1 $
    CloseSharedReferenceScript
      { beaconsBurned = [(scriptHashAsToken refScript,-1)]
      , burnRedeemer = MintBeacon' refScript
      , closeAddress = refVaultAddr
      , closeUTxOs =
          [ ( beaconSymbol
            , lovelaceValueOf 20_000_000 <> singleton beaconSymbol (scriptHashAsToken refScript) 1
            )
          ]
      , newUTxOsAtAddress = []
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  testGroup "Closing Shared Reference Scripts"
    [ checkPredicate "Fail if stake credential does not approve"
        (Test.not assertNoFailedTransactions) closeAsNonOwner
    , checkPredicate "Fail if beacons withdrawn instead of burned"
        (Test.not assertNoFailedTransactions) closeAndWithdrawBeacon
    , checkPredicate "Fail if reference script removed but beacon left behind"
        (Test.not assertNoFailedTransactions) closeButLeaveBeacon
    , checkPredicate "Fail if mint redeemer used to burn"
        (Test.not assertNoFailedTransactions) closeWithMintRedeemer
    , checkPredicate "Allow closing single reference script"
        assertNoFailedTransactions properlyCloseSharedRefScript
    , checkPredicate "Allow closing many reference scripts in one tx"
        assertNoFailedTransactions closeManyRefScripts
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO closeWithMintRedeemer