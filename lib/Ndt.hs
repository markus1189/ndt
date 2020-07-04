module Ndt (trackDependency, updateAllDependencies, updateDependency, deleteDependency) where

import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Aeson (Value)
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           Lens.Micro.Platform ((<&>))
import           Ndt.Fetch
import           Ndt.Sources (lookupDependency, insertDependency, loadSources, saveSources, withSources, removeDependency)
import           Ndt.Types
import           UnliftIO.Async (forConcurrently_)

trackDependency :: ( MonadThrow m
                   , MonadIO m
                   , MonadReader env m
                   , HasNixPrefetchGitAction env
                   , HasNixPrefetchUrlAction env
                   , HasSourcesFile env
                   )
                => DependencyKey
                -> Dependency
                -> m ()
trackDependency dk dep = fetchDependency dep >>= insert dk

updateAllDependencies :: ( MonadUnliftIO m
                         , MonadThrow m
                         , MonadReader env m
                         , HasNixPrefetchGitAction env
                         , HasNixPrefetchUrlAction env
                         , HasSourcesFile env
                         )
                      => m ()
updateAllDependencies = do
  Sources obj <- loadSources
  forConcurrently_ (HM.keys obj) (updateDependency . coerce)

updateDependency :: ( MonadThrow m
                    , MonadIO m
                    , MonadReader env m
                    , HasNixPrefetchGitAction env
                    , HasNixPrefetchUrlAction env
                    , HasSourcesFile env)
                 => DependencyKey
                 -> m ()
updateDependency dk = loadSources >>= lookupDependency dk >>= trackDependency dk

deleteDependency :: ( MonadThrow m
                    , MonadReader env m
                    , HasSourcesFile env
                    , MonadIO m)
                 => DependencyKey
                 -> m ()
deleteDependency dk = withSources (removeDependency dk)

insert :: ( MonadThrow m
          , MonadIO m,
            MonadReader env m
          , HasSourcesFile env
          )
       => DependencyKey
       -> Value
       -> m ()
insert dk v = loadSources <&> insertDependency dk v >>= saveSources
