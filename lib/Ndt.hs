module Ndt (trackDependency, updateAllDependencies, updateDependency) where

import           Control.Monad.Catch (throwM, MonadThrow)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Aeson (Value, Object)
import qualified Data.Aeson as Aeson
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           Lens.Micro.Platform (view, (<&>))
import           Ndt.Fetch
import           Ndt.Sources (lookupDependency, insertDependency, loadSources, saveSources)
import           Ndt.Types
import           UnliftIO.Async (forConcurrently_)

trackDependency :: (MonadThrow m, MonadIO m, MonadReader env m, HasNixPrefetchGitAction env, HasNixPrefetchUrlAction env, HasSourcesFile env) => DependencyKey -> Dependency -> m ()
trackDependency dk dep = fetchDependency dep >>= insert dk

updateAllDependencies :: (MonadUnliftIO m, MonadThrow m, MonadReader env m, HasNixPrefetchGitAction env, HasNixPrefetchUrlAction env, HasSourcesFile env) => m ()
updateAllDependencies = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict @Object sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right obj -> do
      let dks = HM.keys obj
      forConcurrently_ dks (updateDependency . coerce)

updateDependency :: (MonadThrow m, MonadIO m, MonadReader env m, HasNixPrefetchGitAction env, HasNixPrefetchUrlAction env, HasSourcesFile env) => DependencyKey -> m ()
updateDependency dk = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict @Object sources
  case decoded of
    Left msg -> throwM (UnreadableSources sources msg)
    Right json -> do
      dep <- lookupDependency (Sources json) dk
      trackDependency dk dep

insert :: (MonadThrow m, MonadIO m, MonadReader env m, HasSourcesFile env) => DependencyKey -> Value -> m ()
insert dk v = loadSources <&> insertDependency dk v >>= saveSources
