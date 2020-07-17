module Ndt ( trackDependency
           , updateAllDependencies
           , updateDependency
           , deleteDependency
           , showDependency
           , Ndt.renameDependency
           ) where

import qualified Control.Concurrent.MSem as MSem
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Aeson (Value)
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           Data.List (foldl')
import           Data.Text (Text)
import           Lens.Micro.Platform (view)
import           Ndt.Fetch
import           Ndt.Sources (lookupDependency, insertDependency, loadSources, saveSources, withSources, removeDependency, renderDependency, renameDependency)
import           Ndt.Types
import           UnliftIO.Async (forConcurrently)

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
                         , HasJobLimit env
                         , HasNixPrefetchGitAction env
                         , HasNixPrefetchUrlAction env
                         , HasSourcesFile env
                         )
                      => m ()
updateAllDependencies = do
  Sources obj <- loadSources
  updateDependency . coerce . HM.keys $ obj

updateDependency :: ( MonadUnliftIO m
                    , MonadThrow m
                    , MonadReader env m
                    , HasJobLimit env
                    , HasNixPrefetchGitAction env
                    , HasNixPrefetchUrlAction env
                    , HasSourcesFile env)
                 => [DependencyKey]
                 -> m ()
updateDependency dks = do
  js <- view jobLimitL
  srcs <- loadSources
  sem <- liftIO $ MSem.new js
  runInIO <- askRunInIO
  deps <- forConcurrently dks $ \dk -> liftIO $ MSem.with sem (runInIO $ lookupDependency dk srcs >>= fetchDependency)
  insertAll $ zip dks deps

deleteDependency :: ( MonadThrow m
                    , MonadReader env m
                    , HasSourcesFile env
                    , MonadIO m)
                 => DependencyKey
                 -> m ()
deleteDependency dk = withSources (removeDependency dk)

insertAll :: ( MonadThrow m
          , MonadIO m,
            MonadReader env m
          , HasSourcesFile env
          )
       => [(DependencyKey, Value)]
       -> m ()
insertAll dkvs = do
  srcs <- loadSources
  let srcs' = foldl' (\acc (dk,v) -> insertDependency dk v acc) srcs dkvs
  saveSources srcs'

insert :: ( MonadThrow m
          , MonadIO m,
            MonadReader env m
          , HasSourcesFile env
          )
       => DependencyKey
       -> Value
       -> m ()
insert dk v = insertAll [(dk,v)]

showDependency :: ( MonadIO m
                  , MonadThrow m
                  , MonadReader env m
                  , HasSourcesFile env
                  )
               => DependencyKey
               -> m (Maybe Text)
showDependency dk = renderDependency dk <$> loadSources


renameDependency :: ( MonadThrow m
                    , MonadReader env m
                    , HasSourcesFile env
                    , MonadIO m
                    )
                 => DependencyKey
                 -> DependencyKey
                 -> m ()
renameDependency old new = withSources (Ndt.Sources.renameDependency old new)
