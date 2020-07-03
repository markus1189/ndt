module Main where

import Test.Tasty
import Tests.Fetch (fetchTests)
import Tests.Sources (sourcesTests)

tests :: IO TestTree
tests = do
  fts <- fetchTests
  sts <- sourcesTests
  pure $ testGroup "NDT Tests" [fts, sts]

main :: IO ()
main = tests >>= defaultMain
