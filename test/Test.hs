module Main where

import Test.Tasty

import Test.CreateSharedRef as CreateSharedRef
import Test.CloseSharedRef as CloseSharedRef

main :: IO ()
main = defaultMain $ testGroup "Cardano-Reference-Scripts"
  [
    CreateSharedRef.tests,
    CloseSharedRef.tests
  ]