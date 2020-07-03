module Tests.Sources (sourcesTests) where

import           Data.Aeson.Lens (key, _String, _Bool)
import           Data.Coerce (coerce)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Lens.Micro.Platform (ix, (&), (.~))
import           Ndt.Sources (lookupDependency)
import           Ndt.Types (DependencyKey(..), Dependency(..), _Sources)
import           Network.URI (parseAbsoluteURI)
import           Test.Tasty
import           Test.Tasty.Hspec
import qualified Tests.Fixtures as Fixtures

sourcesTests :: IO TestTree
sourcesTests = testSpec "sources" $ do
  let mkUri = fromJust . parseAbsoluteURI
      ndtUri = mkUri "https://github.com/markus1189/ndt"
      ndtUriDefaultNix = mkUri "https://raw.githubusercontent.com/markus1189/ndt/master/default.nix"
  describe "looking up dependencies from the sources" $ do
    it "can parse github dependencies" $ do
      let dk = "ndt"
          branchName = "foo"
          fsm = True
          sources = Fixtures.sources & _Sources . ix dk . key "branch" . _String .~ branchName
                                     & _Sources . ix dk . key "fetchSubmodules" . _Bool .~ fsm
      result <- lookupDependency sources (coerce dk)
      result `shouldBe` GithubDependency ndtUri fsm (T.unpack branchName)

    it "can parse url dependencies" $ do
      let dk = "ndt-default-nix"
          storeName = "foo"
          sources = Fixtures.sources & _Sources . ix dk . key "name" . _String .~ T.pack storeName
      result <- lookupDependency sources (coerce dk)
      result `shouldBe` UrlDependency ndtUriDefaultNix (Just storeName)
