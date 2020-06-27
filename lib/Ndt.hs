module Ndt (trackDependency, updateDependency) where

import qualified Network.URI as URI
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), Value)
import Data.Aeson.Encode.Pretty (Config (..), Indent (Spaces), defConfig, encodePretty')
import Data.Aeson.Lens (_Object, _String, _Bool)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lens.Micro.Platform ((?~))
import Ndt.Fetch
import Ndt.Types
import Network.URI (URI, parseAbsoluteURI)
import RIO
import RIO.Lens

trackDependency :: Dependency -> RIO NdtEnv ()
trackDependency (GithubDependency dk uri fetchSubmodules) = do
  json <- nixPrefetchGit uri fetchSubmodules
  let (owner, repo) = parseOwnerAndRepo uri
      json' =
        json & _Object . at "owner" ?~ Aeson.toJSON owner
          & _Object . at "repo" ?~ Aeson.toJSON repo
          & _Object . at "type" ?~ "github"
  insertDependency dk json'
trackDependency (UrlDependency dk uri storeName) = do
  sha256 <- nixPrefetchUrl uri storeName
  let json =
        Aeson.object $
          [ "url" .= show uri,
            "type" .= ("url" :: Text),
            "sha256" .= T.dropWhileEnd (== '\n') (T.decodeUtf8 (LBS.toStrict sha256))
          ]
            ++ maybe [] (pure . ("name" .=)) storeName
  sources <- view sourcesFileL
  withSources (_Object . at dk ?~ json)

updateDependency :: Text -> RIO NdtEnv ()
updateDependency dk = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict @Value sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right json -> do
      let dep = json ^? _Object . ix dk
      case dep of
        Nothing -> throwM (NoSuchDependency dk)
        Just depValue -> do
          let typ = depValue ^? _Object . ix "type" . _String
              maybeUri = (depValue ^? _Object . ix "url" . _String . to T.unpack) >>= parseAbsoluteURI
              fetchSubmodules = Just True == depValue ^? _Object . ix "fetchSubmodules" . _Bool
              storeName = depValue ^? _Object . ix "name" . _String . to T.unpack
          case maybeUri of
            Nothing -> throwM InvalidGitHubUri
            Just uri ->
              case typ of
                Just "github" -> trackDependency (GithubDependency dk uri fetchSubmodules)
                Just "url" -> trackDependency (UrlDependency dk uri storeName)
                Just t -> throwM (UnknownDependencyType t)
                Nothing -> throwM (UnknownDependencyType "<not present>")

insertDependency :: Text -> Value -> RIO NdtEnv ()
insertDependency dk json = do
  sources <- view sourcesFileL
  withSources (_Object . at dk ?~ json)

withSources :: (Value -> Value) -> RIO NdtEnv ()
withSources f = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right deps -> do
      let deps' = f deps
          encoded = encodePretty' (defConfig {confIndent = Spaces 2}) deps'
      liftIO $ LBS.writeFile sources encoded

parseOwnerAndRepo :: URI -> (String, String)
parseOwnerAndRepo uri = (owner, repo)
  where
    owner = takeWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri -- TODO: extract owner and repo
    repo = takeWhile (/= '/') . dropWhile (== '/') . dropWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri
