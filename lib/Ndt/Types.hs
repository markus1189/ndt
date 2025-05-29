{-# LANGUAGE StrictData #-}
module Ndt.Types
  ( HasSourcesFile (..),
    HasNixPrefetchGitAction (..),
    HasNixPrefetchUrlAction (..),
    HasJobLimit(..),
    NdtException (..),
    Dependency (..),
    DependencyKey(DependencyKey),
    NixPrefetchGitArgs(..),
    NixPrefetchUrlArgs(..),
    Sources(..),
    _Sources
  )
where

import           Control.Exception (Exception)
import           Data.Aeson (Value, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce (coerce)
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro.Platform (Lens', Traversal')
import           Network.URI (URI)
import           Numeric.Natural (Natural)

newtype Sources = Sources (HashMap Text Value) deriving (FromJSON, ToJSON, Show, Eq)
_Sources :: Traversal' Sources (HashMap Text Value)
_Sources f (Sources hm) = Sources <$> f hm

newtype DependencyKey = DependencyKey Text deriving (Show, Eq)

data NixPrefetchGitArgs = NixPrefetchGitArgs URI Bool String

data NixPrefetchUrlArgs = NixPrefetchUrlArgs URI (Maybe String)

class HasSourcesFile cfg where
  sourcesFileL :: Lens' cfg FilePath

class HasNixPrefetchGitAction cfg where
  nixPrefetchGitActionL :: Lens' cfg (NixPrefetchGitArgs -> IO Value)

class HasNixPrefetchUrlAction cfg where
  nixPrefetchUrlActionL :: Lens' cfg (NixPrefetchUrlArgs -> IO LBS.ByteString)

class HasJobLimit cfg where
  jobLimitL :: Lens' cfg Natural

data NdtException
  = NixPrefetchGitFailed Int
  | NixPrefetchGitAesonDecodeError String
  | NixPrefetchUrlFailed Int
  | NoSuchDependency DependencyKey
  | UnreadableSources FilePath String
  | InvalidGitHubUri DependencyKey
  | MissingFieldInSources Text
  | UnknownDependencyType DependencyKey Text

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
