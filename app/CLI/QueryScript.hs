{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.QueryScript
(
  runQuery
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi
import CLI.Types
import CardanoReferenceScripts (CurrencySymbol,ScriptHash)

-- | Takes the beacon symbol, the target script, and the desired network to query the relevant
-- off-chain api endpoint.
runQuery :: CurrencySymbol -> ScriptHash -> Network -> IO AvailableReference
runQuery beaconSym targetRefScript network = do
  manager' <- newManager tlsManagerSettings
  case network of
    PreProdTestnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (queryBlockfrost apiKey' beaconSym targetRefScript) env
      case res of
        Right r -> return r
        Left err -> throw err
    Mainnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-mainnet.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (queryBlockfrost apiKey' beaconSym targetRefScript) env
      case res of
        Right r -> return r
        Left err -> throw err