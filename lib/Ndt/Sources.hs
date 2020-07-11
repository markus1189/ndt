module Ndt.Sources ( lookupDependency
                   , insertDependency
                   , loadSources
                   , saveSources
                   , withSources
                   , removeDependency
                   , listDependencies
                   , renderDependency
                   , renameDependency
                   ) where

import           Control.Monad.Catch (throwM, MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (Config (..), Indent (Spaces), defConfig, encodePretty')
import           Data.Aeson.Lens (_Bool, _Object, _String)
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Lens.Micro.Platform (at, ix, to, (^?), (&), (?~), (.~), view)
import           Ndt.Types
import           Network.URI (parseAbsoluteURI)

lookupDependency :: MonadThrow m => DependencyKey -> Sources -> m Dependency
lookupDependency dk (Sources json) = do
  let dep = json ^? ix (coerce dk)
  case dep of
    Nothing -> throwM (NoSuchDependency dk)
    Just depValue -> do
      let typ = depValue ^? _Object . ix "type" . _String
          maybeUri = (depValue ^? _Object . ix "url" . _String . to T.unpack) >>= parseAbsoluteURI
          fetchSubmodules = Just True == depValue ^? _Object . ix "fetchSubmodules" . _Bool
          branchName = maybe "master" T.unpack $ depValue ^? _Object . ix "branch" . _String
          storeName = depValue ^? _Object . ix "name" . _String . to T.unpack
      case maybeUri of
        Nothing -> throwM (InvalidGitHubUri dk)
        Just uri ->
          case typ of
            Just "github" -> pure (GithubDependency uri fetchSubmodules branchName)
            Just "url" ->  pure (UrlDependency uri storeName)
            Just t -> throwM (UnknownDependencyType dk t)
            Nothing -> throwM (UnknownDependencyType dk "<not present>")

insertDependency :: DependencyKey -> Value -> Sources -> Sources
insertDependency dk value srcs = srcs & _Sources . at (coerce dk) ?~ value

loadSources :: (MonadIO m, MonadThrow m, MonadReader env m, HasSourcesFile env) => m Sources
loadSources = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right srcs -> pure srcs

saveSources :: (MonadReader env m, HasSourcesFile env, MonadIO m) => Sources -> m ()
saveSources (Sources sources) = do
  file <- view sourcesFileL
  let encoded = encodePretty' prettyConfig sources
  liftIO $ LBS.writeFile file encoded

withSources :: (MonadThrow m, MonadReader env m, HasSourcesFile env, MonadIO m) => (Sources -> Sources) -> m ()
withSources f = loadSources >>= saveSources . f

removeDependency :: DependencyKey -> Sources -> Sources
removeDependency dk srcs = srcs & _Sources . at (coerce dk) .~ Nothing

listDependencies :: (MonadIO m, MonadThrow m, MonadReader env m, HasSourcesFile env) => m [DependencyKey]
listDependencies = sortOn (coerce @DependencyKey @Text) . (\(Sources hm) -> coerce (HM.keys hm)) <$> loadSources

renderDependency :: DependencyKey -> Sources -> Maybe Text
renderDependency dk srcs = encode <$> maybeDep
  where encode = T.decodeUtf8 . LBS.toStrict . encodePretty' prettyConfig
        maybeDep = srcs ^? _Sources . ix (coerce dk)

prettyConfig :: Config
prettyConfig = defConfig {confIndent = Spaces 2, confCompare = compare }

renameDependency :: DependencyKey -> DependencyKey -> Sources -> Sources
renameDependency dkOld dkNew srcs = srcs & _Sources . at (coerce dkNew) .~ oldDep
                                         & _Sources . at (coerce dkOld) .~ Nothing
  where oldDep = srcs ^? _Sources . ix (coerce dkOld)
