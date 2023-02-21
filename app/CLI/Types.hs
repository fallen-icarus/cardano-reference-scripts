{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Types where

import Data.Aeson
import CardanoReferenceScripts

data Command 
  = ExportBeaconPolicy !FilePath
  | CreateBeaconRedeemer !BeaconRedeemer !FilePath
  | ExportHelperScript !FilePath
  | CreateDatum !FilePath
  | QueryScript !ScriptHash !Network !Output

-- | For when saving to file is optional
data Output = Stdout | File !FilePath

data Network 
  = Mainnet String  -- ^ Api key
  | PreProdTestnet String  -- ^ Api key

-- | Type that captures all info a user needs to interact with available reference scripts.
newtype AvailableReference = AvailableReference { referenceScriptTxIx :: String } deriving (Show)

instance ToJSON AvailableReference where
  toJSON (AvailableReference txIx) = object ["reference_script_tx_ix" .= txIx]