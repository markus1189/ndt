module Tests.Fetch (fetchTests) where

import Test.Tasty
import Test.Tasty.Hspec

fetchTests :: IO TestTree
fetchTests = testSpec "fetch" $ do
  describe "the github fetcher" $ do
    it "foo" $ do
      1 `shouldBe` (1::Int)
