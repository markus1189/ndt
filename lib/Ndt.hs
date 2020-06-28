module Ndt (trackDependency, updateDependency) where

import           Data.Aeson ((.=), Value)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (Config (..), Indent (Spaces), defConfig, encodePretty')
import           Data.Aeson.Lens (_Bool, _Object, _String)
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Lens.Micro.Platform ((?~))
import           Ndt.Fetch
import           Ndt.Types
import           Network.URI (URI, parseAbsoluteURI)
import qualified Network.URI as URI
import           RIO
import           RIO.Lens

trackDependency :: DependencyKey -> Dependency -> RIO NdtEnv ()
trackDependency dk (GithubDependency uri fetchSubmodules) = do
  json <- nixPrefetchGit uri fetchSubmodules
  let (owner, repo) = parseOwnerAndRepo uri
      json' =
        json & _Object . at "owner" ?~ Aeson.toJSON owner
          & _Object . at "repo" ?~ Aeson.toJSON repo
          & _Object . at "type" ?~ "github"
  insertDependency dk json'
trackDependency dk (UrlDependency uri storeName) = do
  sha256 <- nixPrefetchUrl uri storeName
  let json =
        Aeson.object $
          [ "url" .= show uri,
            "type" .= ("url" :: Text),
            "sha256" .= T.dropWhileEnd (== '\n') (T.decodeUtf8 (LBS.toStrict sha256))
          ]
            ++ maybe [] (pure . ("name" .=)) storeName
  insertDependency dk json

updateDependency :: DependencyKey -> RIO NdtEnv ()
updateDependency dk = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict @Value sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right json -> do
      let dep = json ^? _Object . ix (coerce dk)
      case dep of
        Nothing -> throwM (NoSuchDependency dk)
        Just depValue -> do
          let typ = depValue ^? _Object . ix "type" . _String
              maybeUri = (depValue ^? _Object . ix "url" . _String . to T.unpack) >>= parseAbsoluteURI
              fetchSubmodules = Just True == depValue ^? _Object . ix "fetchSubmodules" . _Bool
              storeName = depValue ^? _Object . ix "name" . _String . to T.unpack
          case maybeUri of
            Nothing -> throwM (InvalidGitHubUri dk)
            Just uri ->
              case typ of
                Just "github" -> trackDependency dk (GithubDependency uri fetchSubmodules)
                Just "url" -> trackDependency dk (UrlDependency uri storeName)
                Just t -> throwM (UnknownDependencyType dk t)
                Nothing -> throwM (UnknownDependencyType dk "<not present>")

insertDependency :: DependencyKey -> Value -> RIO NdtEnv ()
insertDependency dk json = withSources (_Object . at (coerce dk) ?~ json)

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
    owner = takeWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri
    repo = takeWhile (/= '/') . dropWhile (== '/') . dropWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri
