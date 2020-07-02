{-# LANGUAGE StrictData #-}
module Ndt.Types
  ( HasSourcesFile (..),
    HasNixPrefetchGitAction (..),
    HasNixPrefetchUrlAction (..),
    NdtEnv (NdtEnv),
    NdtException (..),
    Dependency (..),
    DependencyKey(DependencyKey),
    NixPrefetchGitArgs(..),
    NixPrefetchUrlArgs(..)
  )
where

import Data.Coerce (coerce)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Network.URI (URI)
import RIO

newtype DependencyKey = DependencyKey Text deriving (Show, Eq)

data NixPrefetchGitArgs = NixPrefetchGitArgs URI Bool String

data NixPrefetchUrlArgs = NixPrefetchUrlArgs URI (Maybe String)

class HasSourcesFile cfg where
  sourcesFileL :: Lens' cfg FilePath

class HasNixPrefetchGitAction cfg where
  nixPrefetchGitActionL :: Lens' cfg (NixPrefetchGitArgs -> IO Value)

class HasNixPrefetchUrlAction cfg where
  nixPrefetchUrlActionL :: Lens' cfg (NixPrefetchUrlArgs -> IO LBS.ByteString)

data NdtEnv
  = NdtEnv
      { _ndtEnvSourcesFile :: FilePath,
        _ndtEnvNixPrefetchGitAction :: NixPrefetchGitArgs -> IO Value,
        _ndtEnvNixPrefetchUrlAction :: NixPrefetchUrlArgs -> IO LBS.ByteString
      }

instance HasSourcesFile NdtEnv where
  sourcesFileL = lens _ndtEnvSourcesFile (\x y -> x {_ndtEnvSourcesFile = y})

instance HasNixPrefetchGitAction NdtEnv where
  nixPrefetchGitActionL = lens _ndtEnvNixPrefetchGitAction (\x y -> x {_ndtEnvNixPrefetchGitAction = y})

instance HasNixPrefetchUrlAction NdtEnv where
  nixPrefetchUrlActionL = lens _ndtEnvNixPrefetchUrlAction (\x y -> x {_ndtEnvNixPrefetchUrlAction = y})

data NdtException
  = NixPrefetchGitFailed Int
  | NixPrefetchGitAesonDecodeError String
  | NixPrefetchUrlFailed Int
  | NoSuchDependency DependencyKey
  | UnreadableSources FilePath String
  | InvalidGitHubUri DependencyKey
  | MissingFieldInSources Text
  | UnknownDependencyType DependencyKey Text
  deriving (Typeable)

instance Exception NdtException

instance Show NdtException where
  show (NixPrefetchGitFailed ec) = "prefetching the git repository failed with exit code: " <> show ec
  show (NixPrefetchGitAesonDecodeError msg) = "could not parse the output of nix-prefetch-git: " <> msg
  show (NixPrefetchUrlFailed ec) = "prefetching the url failed with exit code: " <> show ec
  show (NoSuchDependency dk) = "could not find dependency: " <> T.unpack (coerce dk)
  show (UnreadableSources fp msg) = "could not read " <> fp <> ": " <> msg
  show (InvalidGitHubUri dk) = "invalid/missing github uri for dependency: " <> T.unpack (coerce dk)
  show (MissingFieldInSources key) = "INTERNAL ERROR: could not find field in sources: " <> T.unpack key
  show (UnknownDependencyType dk typ) = "Unknown dependency type: '" <> T.unpack typ <> "' for dependency: " <> T.unpack (coerce dk)

data Dependency
  = GithubDependency
      { _ghDepGithubUrl :: URI,
        _ghDepFetchSubmodules :: Bool,
        _ghDepBranchName :: String
      }
  | UrlDependency
      { _urlDepUri :: URI,
        _urlDepStoreName :: Maybe String
      }
  deriving (Eq, Show)
