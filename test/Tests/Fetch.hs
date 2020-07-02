module Tests.Fetch (fetchTests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

fetchTests :: IO TestTree
fetchTests = testSpec "fetch" $ do
  describe "the github fetcher" $ do
    it "foo" $ do
      1 `shouldBe` 2
