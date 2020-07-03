module Ndt.Sources (parseDependency) where

import           Control.Monad.Catch (throwM, MonadThrow)
import           Data.Aeson.Lens (_Bool, _Object, _String)
import           Data.Coerce (coerce)
import qualified Data.Text as T
import           Lens.Micro.Platform (ix, to, (^?))
import           Ndt.Types
import           Network.URI (parseAbsoluteURI)

parseDependency :: MonadThrow m => Sources -> DependencyKey -> m Dependency
parseDependency (Sources json) dk = do
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
