{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey(..),
  queryBlockfrost
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T

import CLI.Types
import CardanoReferenceScripts (CurrencySymbol,TokenName,ScriptHash,scriptHashAsToken)

-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Wrapper around the beacon policy id
newtype BeaconId = BeaconId (CurrencySymbol,TokenName)

instance ToHttpApiData BeaconId where
  toQueryParam (BeaconId (sym,name)) = T.pack 
                                     $ show sym <> drop 2 (show name)  -- ^ drop prefix

-- | An address that contains a beacon.
-- The response type of the beaconAddressList api
newtype BeaconAddress = BeaconAddress { unBeaconAddress :: String } deriving (Show)

instance FromJSON BeaconAddress where
  parseJSON (Object o) = BeaconAddress <$> o .: "address"
  parseJSON _ = mzero

instance ToHttpApiData BeaconAddress where
  toQueryParam = T.pack . unBeaconAddress

-- | The response type of the addressUTxOs api
data RawUTxOInfo = RawUTxOInfo
  { rawTxHash :: String
  , rawIx :: Integer
  , rawReferenceScriptHash :: Maybe String
  } deriving (Show)

instance FromJSON RawUTxOInfo where
  parseJSON (Object o) =
    RawUTxOInfo
      <$> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "reference_script_hash"
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "addresses"
    :> QueryParam' '[Required] "count" Integer
    :> Get '[JSON] [BeaconAddress]

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" BeaconAddress
    :> "utxos"
    :> Get '[JSON] [RawUTxOInfo]

beaconAddressListApi :<|> addressUTxOsApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Query Blockfrost Function
-------------------------------------------------
queryBlockfrost :: BlockfrostApiKey -> CurrencySymbol -> ScriptHash -> ClientM (Maybe AvailableReference)
queryBlockfrost apiKey beaconSym targetScriptHash = do
    let beaconId = BeaconId (beaconSym,scriptHashAsToken targetScriptHash)
    -- | Get the first address that currently holds that beacon.
    addr <- beaconAddressListApi apiKey beaconId 1
    if null addr
    then return Nothing
    else do
      -- | Get all of the utxos for that address.
      addrUtxos <- addressUTxOsApi apiKey $ head addr

      return $ Just $ getInfo addrUtxos $ show targetScriptHash

  where
    getInfo :: [RawUTxOInfo] -> String -> AvailableReference
    getInfo [] _ = error "Reference script not found."
    getInfo ((RawUTxOInfo _ _ Nothing):xs) target = getInfo xs target
    getInfo ((RawUTxOInfo tx ix (Just rs)):xs) target
      | rs == target = AvailableReference $ tx <> "#" <> show ix 
      | otherwise = getInfo xs target