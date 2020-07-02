module Ndt.Fetch
  ( nixPrefetchGitProcess,
    nixPrefetchGit,
    nixPrefetchUrlProcess,
    nixPrefetchUrl,
  )
where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as LBS
import Ndt.Types
import RIO
import RIO.List (stripPrefix)
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

nixPrefetchGit :: NixPrefetchGitArgs -> RIO NdtEnv Value
nixPrefetchGit args = do
  action <- view nixPrefetchGitActionL
  liftIO $ action args

nixPrefetchUrl :: NixPrefetchUrlArgs -> RIO NdtEnv LBS.ByteString
nixPrefetchUrl args = do
  action <- view nixPrefetchUrlActionL
  liftIO $ action args
