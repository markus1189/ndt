module Ndt (trackDependency, updateAllDependencies, updateDependency) where

import           Control.Monad.Catch (throwM, MonadThrow)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Aeson (Value, Object)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (Config (..), Indent (Spaces), defConfig, encodePretty')
import           Data.Aeson.Lens (_Object)
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import           Lens.Micro.Platform (view, at, (?~))
import           Ndt.Fetch
import           Ndt.Sources (parseDependency)
import           Ndt.Types

trackDependency :: (MonadThrow m, MonadIO m, MonadReader env m, HasNixPrefetchGitAction env, HasNixPrefetchUrlAction env, HasSourcesFile env) => DependencyKey -> Dependency -> m ()
trackDependency dk dep = fetchDependency dep >>= insertDependency dk

updateAllDependencies :: (MonadThrow m, MonadIO m, MonadReader env m, HasNixPrefetchGitAction env, HasNixPrefetchUrlAction env, HasSourcesFile env) => m ()
updateAllDependencies = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict @Object sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right obj -> do
      let dks = HM.keys obj
      for_ dks (updateDependency . coerce)

updateDependency :: (MonadThrow m, MonadIO m, MonadReader env m, HasNixPrefetchGitAction env, HasNixPrefetchUrlAction env, HasSourcesFile env) => DependencyKey -> m ()
updateDependency dk = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict @Object sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right json -> do
      dep <- parseDependency (Sources json) dk
      trackDependency dk dep

insertDependency :: (MonadThrow m, MonadIO m, MonadReader env m, HasSourcesFile env) => DependencyKey -> Value -> m ()
insertDependency dk json = withSources (_Object . at (coerce dk) ?~ json)

withSources :: (MonadThrow m, MonadIO m, MonadReader env m, HasSourcesFile env) => (Value -> Value) -> m ()
withSources f = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right deps -> do
      let deps' = f deps
          encoded = encodePretty' (defConfig {confIndent = Spaces 2}) deps'
      liftIO $ LBS.writeFile sources encoded
