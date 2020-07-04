module Tests.Sources (sourcesTests) where

import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (key, _String, _Bool)
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro.Platform (at, ix, (&), (.~), (?~), (^?))
import           Ndt.Sources (lookupDependency, insertDependency, removeDependency)
import           Ndt.Types (DependencyKey(..), Dependency(..), _Sources, Sources(..))
import           Network.URI (parseAbsoluteURI)
import           Test.Tasty
import           Test.Tasty.Hspec
import qualified Tests.Fixtures as Fixtures

sourcesTests :: IO TestTree
sourcesTests = testSpec "sources" $ do
  let mkUri = fromJust . parseAbsoluteURI
      ndtUri = mkUri "https://github.com/markus1189/ndt"
      ndtUriDefaultNix = mkUri "https://raw.githubusercontent.com/markus1189/ndt/master/default.nix"
  describe "lookupDependency" $ do
    it "can parse github dependencies" $ do
      let dk = "ndt"
          branchName = "foo"
          fsm = True
          sources = Fixtures.sources & _Sources . ix dk . key "branch" . _String .~ branchName
                                     & _Sources . ix dk . key "fetchSubmodules" . _Bool .~ fsm
      result <- lookupDependency (coerce dk) sources
      result `shouldBe` GithubDependency ndtUri fsm (T.unpack branchName)

    it "can parse url dependencies" $ do
      let dk = "ndt-default-nix"
          storeName = "foo"
          sources = Fixtures.sources & _Sources . ix dk . key "name" . _String .~ T.pack storeName
      result <- lookupDependency (coerce dk) sources
      result `shouldBe` UrlDependency ndtUriDefaultNix (Just storeName)

  describe "insertDependency" $ do
    it "inserts the dependency if not yet present" $ do
      let dk = "foo"
          srcs = Sources HM.empty
          result = insertDependency (coerce dk) (Aeson.object []) srcs
      result ^? _Sources . ix dk `shouldBe` Just (Aeson.object [])

  describe "insertDependency" $ do
    it "overwrites the dependency if already present" $ do
      let dk = "foo"
          srcs = Sources HM.empty & _Sources . at dk ?~ "garbage"
          result = insertDependency (coerce dk) (Aeson.object []) srcs
      result ^? _Sources . ix dk `shouldBe` Just (Aeson.object [])

  describe "removeDependency" $ do
    it "does nothing if key not present" $ do
      let dk = ("foo" :: Text)
          srcs = Fixtures.sources
          result = removeDependency (coerce dk) srcs
      result `shouldBe` srcs

    it "removes a dependency" $ do
      let dk = ("foo" :: Text)
          srcs = Sources $ HM.empty & at dk ?~ Fixtures.ndtSource
          result = removeDependency (coerce dk) srcs
      srcs ^? _Sources . ix dk `shouldBe` Just Fixtures.ndtSource
      result ^? _Sources . ix dk `shouldBe` Nothing
