module CLI.Run
(
  runCommand
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import CardanoReferenceScripts
import CLI.Types
import CLI.QueryScript

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportBeaconPolicy file -> exportPolicy file
  CreateBeaconRedeemer r file -> createRedeemer r file
  ExportHelperScript file -> exportHelperScript file
  CreateDatum file -> createDatum file
  QueryScript scriptHash network output -> runQueryScript scriptHash network output

runQueryScript :: ScriptHash -> Network -> Output -> IO ()
runQueryScript scriptHash network output = do
  res <- runQuery beaconSymbol scriptHash network
  case output of
    Stdout -> BL.putStr $ encode res
    File file -> BL.writeFile file $ encodePretty res

createRedeemer :: BeaconRedeemer -> FilePath -> IO ()
createRedeemer r file = do
  writeData file r
  putStrLn "Beacon redeemer created successfully."

exportPolicy :: FilePath -> IO ()
exportPolicy file = do
  res <- writeScript file beaconScript
  case res of
    Right _ -> putStrLn "Beacon policy exported successfully."
    Left err -> putStrLn $ "There was an error: " <> show err

exportHelperScript :: FilePath -> IO ()
exportHelperScript file = do
  res <- writeScript file referenceVaultScript
  case res of
    Right _ -> putStrLn "Helper script exported successfully."
    Left err -> putStrLn $ "There was an error: " <> show err

createDatum :: FilePath -> IO ()
createDatum file = do
  writeData file beaconSymbol
  putStrLn "Helper script's datum created successfully."