module Main where

import Test.Tasty
import Tests.Fetch (fetchTests)

tests :: IO TestTree
tests = do
  ts <- fetchTests
  pure $ testGroup "NDT Tests" [ts]

main :: IO ()
main = tests >>= defaultMain
