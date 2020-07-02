module Main where

import Test.Tasty
import Tests.Fetch (fetchTests)

tests :: IO TestTree
tests = do
  ft <- fetchTests
  pure $ testGroup "NDT" [ft]

main :: IO ()
main = tests >>= defaultMain
