module Tests.Fetch (fetchTests) where

import           Test.Tasty
import           Test.Tasty.Hspec

import           Control.Monad.Reader (runReaderT)
import           Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (key, _Object, _Bool, _String)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Lens.Micro.Platform ((^?), (^?!), lens, to)
import           Ndt.Fetch (fetchDependency)
import           Ndt.Types (Dependency(..), HasNixPrefetchGitAction(..), HasNixPrefetchUrlAction(..), NixPrefetchGitArgs(..), NixPrefetchUrlArgs(..))
import           Network.URI (parseAbsoluteURI)

data TestEnv = TestEnv (NixPrefetchGitArgs -> IO Value) (NixPrefetchUrlArgs -> IO LBS.ByteString)

instance HasNixPrefetchGitAction TestEnv where
  nixPrefetchGitActionL = lens (\(TestEnv git _) -> git) (\(TestEnv _ url) git -> TestEnv git url)

instance HasNixPrefetchUrlAction TestEnv where
  nixPrefetchUrlActionL = lens (\(TestEnv _ url) -> url) (\(TestEnv git _) url -> TestEnv git url)

emptyFetchOutput :: TestEnv
emptyFetchOutput = TestEnv (const (pure (Aeson.object []))) (const (pure ""))

plausibleOutput :: TestEnv
plausibleOutput = TestEnv
                     (pure . json)
                     (const (pure "0000000000000000000000000000000000000000000000000000"))
  where json (NixPrefetchGitArgs _ fsm _) =
          Aeson.object [ "url" .= ("https://github.com/markus1189/ndt" :: String)
                       , "rev" .= ("b662443ec1e5152aaf2166a391f4a582117f0fd4" :: String)
                       , "date" .= ("2020-07-02T20:16:00+02:00" :: String)
                       , "path" .= ("/nix/store/i3ssqhg14pcsagcb3mgy4c86n72rhnqs-ndt" :: String)
                       , "sha256" .= ("0v38qqpsagl598n7wbingv3737x7g91xzqci6vzmvfqs8q5wxcbb" :: String)
                       , "fetchSubmodules" .= fsm
                       , "deepClone" .= False
                       , "leaveDotGit" .= False
                       ]


fetchTests :: IO TestTree
fetchTests = testSpec "fetch" $ do
  describe "the github fetcher" $ do
    it "adds owner and repo from the uri" $ do
      result <- runReaderT (fetchDependency (GithubDependency ndtUri False "master")) emptyFetchOutput
      result ^? key "owner" `shouldBe` Just "markus1189"
      result ^? key "repo" `shouldBe` Just "ndt"

    it "remembers the args given" $ do
      let fetchSubmodules = True
          branch = "foo"
      result <- runReaderT (fetchDependency (GithubDependency ndtUri fetchSubmodules branch)) plausibleOutput
      result ^? key "fetchSubmodules" . _Bool `shouldBe` Just fetchSubmodules
      result ^? key "branch" . _String `shouldBe` Just (T.pack branch)

    it "keeps all of the original attributes" $ do
      result <- runReaderT (fetchDependency (GithubDependency ndtUri False "master")) plausibleOutput
      let keys = result ^?! _Object . to HM.keys
      keys `shouldContainAllOf` [ "url"
                                , "rev"
                                , "date"
                                , "path"
                                , "sha256"
                                , "fetchSubmodules"
                                , "deepClone"
                                , "leaveDotGit"
                                ]

    it "adds some custom attributes" $ do
      result <- runReaderT (fetchDependency (GithubDependency ndtUri False "master")) plausibleOutput
      let keys = result ^?! _Object . to HM.keys
      keys `shouldContainAllOf` [ "owner"
                                , "repo"
                                , "type"
                                , "branch"
                                ]

  describe "the url fetcher" $ do
    it "adds some custom attributes" $ do
      let storeName = Just "my-custom-store-name"
      result <- runReaderT (fetchDependency (UrlDependency ndtUri storeName)) plausibleOutput
      let keys = result ^?! _Object . to HM.keys
      keys `shouldMatchList` ["url", "type", "sha256", "name"]
      result ^? key "type" . _String `shouldBe` Just "url"
      result ^? key "name" . _String `shouldBe` T.pack <$> storeName

  where ndtUri = fromJust $ parseAbsoluteURI "https://github.com/markus1189/ndt"

shouldContainAllOf :: (Show a, Eq a) => [a] -> [a] -> Expectation
shouldContainAllOf actual expected = actual `shouldSatisfy` (\ks -> all (`elem` ks) expected)
