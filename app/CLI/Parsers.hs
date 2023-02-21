module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative

import CardanoReferenceScripts
import CLI.Types

-------------------------------------------------
-- Main Parser
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "export-policy"
      (info pExportBeaconPolicy $ progDesc "Export the reference script beacon policy.")
  , command "redeemer"
      (info pCreateBeaconRedeemer $ progDesc "Create the redeemer for the reference script beacon policy.")
  , command "export-helper-script"
      (info pExportHelperScript $ progDesc "Export the helper script.")
  , command "datum"
      (info pCreateDatum $ progDesc "Create the datum for the helper script.")
  , command "query"
      (info parseQueryScript $ progDesc "Query the chain for a reference script.")
  ]

-------------------------------------------------
-- Query Parsers
-------------------------------------------------
parseQueryScript :: Parser Command
parseQueryScript = 
    QueryScript
      <$> pScriptHash
      <*> pNetwork
      <*> pOutput
  where
    pScriptHash :: Parser ScriptHash
    pScriptHash = option (eitherReader readScriptHash)
      (  long "script-hash" 
      <> metavar "STRING" 
      <> help "Script hash to lookup."
      )

    pNetwork :: Parser Network
    pNetwork = pMainnet <|> pPreProdTestnet
      where
        pMainnet :: Parser Network
        pMainnet = Mainnet <$> strOption
          (  long "mainnet"
          <> metavar "STRING"
          <> help "Query the mainnet using the Blockfrost Api with the supplied api key.")
        
        pPreProdTestnet :: Parser Network
        pPreProdTestnet = PreProdTestnet <$> strOption
          (  long "preprod-testnet"
          <> metavar "STRING"
          <> help "Query the preproduction testnet using the Blockfrost Api with the supplied api key.")

    pOutput :: Parser Output
    pOutput = pStdOut <|> File <$> pOutputFile
      where
        pStdOut :: Parser Output
        pStdOut = flag' Stdout
          (  long "stdout"
          <> help "Display to stdout."
          )

-------------------------------------------------
-- Beacon Parsers
-------------------------------------------------
pExportBeaconPolicy :: Parser Command
pExportBeaconPolicy = ExportBeaconPolicy <$> pOutputFile

pCreateBeaconRedeemer :: Parser Command
pCreateBeaconRedeemer = 
    CreateBeaconRedeemer
      <$> (pMint <|> pBurn)
      <*> pOutputFile
  where
    pMint :: Parser BeaconRedeemer
    pMint = MintBeacon <$> option (eitherReader readScriptHash)
      (  long "mint-beacon" 
      <> metavar "STRING" 
      <> help "Mint a reference script beacon for the supplied script hash."
      )

    pBurn :: Parser BeaconRedeemer
    pBurn = flag' BurnBeacon
      (  long "burn-beacon"
      <> help "Burn a reference script beacon."
      )

pExportHelperScript :: Parser Command
pExportHelperScript = ExportHelperScript <$> pOutputFile

pCreateDatum :: Parser Command
pCreateDatum = CreateDatum <$> pOutputFile

-------------------------------------------------
-- Misc Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )