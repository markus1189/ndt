module Main where

import Test.Tasty
import Test.Tasty.Hspec

tests :: IO TestTree
tests = do
  ts <- ndtTests
  pure $ testGroup "NDT Tests" [ts]

main :: IO ()
main = tests >>= defaultMain

ndtTests :: IO TestTree
ndtTests = testSpec "ndt-hspec" $ do
  describe "the github fetcher" $ do
    it "foo" $ do
      1 `shouldBe` (1::Int)
