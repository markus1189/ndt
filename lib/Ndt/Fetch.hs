module Ndt.Fetch
  ( nixPrefetchGitProcess,
    nixPrefetchGit,
    nixPrefetchUrlProcess,
    nixPrefetchUrl,
  )
where

import           Control.Monad.Catch (throwM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import           Lens.Micro.Platform (view)
import           Ndt.Types
import           System.Exit (ExitCode(..))
import qualified System.Process.Typed as Process

nixPrefetchGitProcess :: NixPrefetchGitArgs -> IO Value
nixPrefetchGitProcess (NixPrefetchGitArgs uri fetchSubmodules branchName) = do
  (ec, output) <-
    Process.readProcessStdout $
      Process.proc
        "nix-prefetch-git"
        ("--rev" : ("refs/heads/" <> fromMaybe branchName (stripPrefix "refs/heads/" branchName)) : show uri : ["--fetch-submodules" | fetchSubmodules])
  case ec of
    ExitSuccess -> case Aeson.eitherDecode' @Value output of
      Left e -> throwM (NixPrefetchGitAesonDecodeError e)
      Right json -> return json
    ExitFailure i -> throwM (NixPrefetchGitFailed i)

nixPrefetchUrlProcess :: NixPrefetchUrlArgs -> IO LBS.ByteString
nixPrefetchUrlProcess (NixPrefetchUrlArgs uri maybeStoreName) = do
  (ec, output) <-
    Process.readProcessStdout $
      Process.proc
        "nix-prefetch-url"
        (show uri : (["--type", "sha256"] ++ maybe [] (("--name" :) . pure) maybeStoreName))
  case ec of
    ExitSuccess -> return output
    ExitFailure i -> throwM (NixPrefetchUrlFailed i)

nixPrefetchGit :: (MonadIO m, MonadReader env m, HasNixPrefetchGitAction env) => NixPrefetchGitArgs -> m Value
nixPrefetchGit args = do
  action <- view nixPrefetchGitActionL
  liftIO $ action args

nixPrefetchUrl :: (MonadIO m, MonadReader env m, HasNixPrefetchUrlAction env) => NixPrefetchUrlArgs -> m LBS.ByteString
nixPrefetchUrl args = do
  action <- view nixPrefetchUrlActionL
  liftIO $ action args
