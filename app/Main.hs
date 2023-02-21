module Main where

import Options.Applicative

import CLI.Parsers (parseCommand)
import CLI.Run

main :: IO ()
main = do
  let preferences = prefs $ showHelpOnError <> showHelpOnEmpty
      opts = info (parseCommand <**> helper) (fullDesc <> progDesc "An application for trustlessly sharing reference scripts.")
  customExecParser preferences opts >>= runCommand