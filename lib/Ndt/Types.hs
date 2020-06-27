module Ndt.Types
  ( HasSourcesFile (..),
    HasNixPrefetchGitAction (..),
    HasNixPrefetchUrlAction (..),
    NdtEnv (NdtEnv),
    NdtException (..),
    Dependency (..),
  )
where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Network.URI (URI)
import RIO

class HasSourcesFile cfg where
  sourcesFileL :: Lens' cfg FilePath

class HasNixPrefetchGitAction cfg where
  nixPrefetchGitActionL :: Lens' cfg (URI -> Bool -> IO Value)

class HasNixPrefetchUrlAction cfg where
  nixPrefetchUrlActionL :: Lens' cfg (URI -> Maybe String -> IO LBS.ByteString)

data NdtEnv
  = NdtEnv
      { _ndtEnvSourcesFile :: FilePath,
        _ndtEnvNixPrefetchGitAction :: URI -> Bool -> IO Value,
        _ndtEnvNixPrefetchUrlAction :: URI -> Maybe String -> IO LBS.ByteString
      }

instance HasSourcesFile NdtEnv where
  sourcesFileL = lens _ndtEnvSourcesFile (\x y -> x {_ndtEnvSourcesFile = y})

instance HasNixPrefetchGitAction NdtEnv where
  nixPrefetchGitActionL = lens _ndtEnvNixPrefetchGitAction (\x y -> x {_ndtEnvNixPrefetchGitAction = y})

instance HasNixPrefetchUrlAction NdtEnv where
  nixPrefetchUrlActionL = lens _ndtEnvNixPrefetchUrlAction (\x y -> x {_ndtEnvNixPrefetchUrlAction = y})

data NdtException
  = NixPrefetchGitFailed Int
  | NixPrefetchUrlFailed Int
  | NixPrefetchGitAesonDecodeError String
  | NoSuchDependency Text
  | UnreadableSources FilePath String
  | InvalidGitHubUri
  | MissingFieldInSources Text
  deriving (Typeable)

instance Exception NdtException

-- TODO: add dependencykeys
instance Show NdtException where
  show (NixPrefetchGitFailed ec) = "prefetching the git repository failed with exit code: " <> show ec
  show (NixPrefetchGitAesonDecodeError msg) = "could not parse the output of nix-prefetch-git: " <> msg
  show (NixPrefetchUrlFailed ec) = "prefetching the url failed with exit code: " <> show ec
  show (NoSuchDependency dep) = "could not find dependency: " <> T.unpack dep
  show (UnreadableSources fp msg) = "could not read " <> fp <> ": " <> msg
  show InvalidGitHubUri = "invalid/missing github uri"
  show (MissingFieldInSources key) = "INTERNAL ERROR: could not find field in sources: " <> T.unpack key

data Dependency
  = GithubDependency
      { _ghDepDependencyKey :: Text,
        _ghDepGithubUrl :: URI,
        _ghDepFetchSubmodules :: Bool
      }
  | UrlDependency
      { _urlDepDependencyKey :: Text,
        _urlDepUri :: URI,
        _urlDepStoreName :: Maybe String
      }
  deriving (Show)
