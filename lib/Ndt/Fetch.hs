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
import Network.URI (URI)
import RIO
import qualified System.Process.Typed as Process

nixPrefetchGitProcess :: URI -> Bool -> IO Value
nixPrefetchGitProcess uri fetchSubmodules = do
  (ec, output) <-
    Process.readProcessStdout $
      Process.proc
        "nix-prefetch-git"
        (show uri : ["--fetch-submodules" | fetchSubmodules])
  case ec of
    ExitSuccess -> case Aeson.eitherDecode' @Value output of
      Left e -> throwM (NixPrefetchGitAesonDecodeError e)
      Right json -> return json
    ExitFailure i -> throwM (NixPrefetchGitFailed i)

nixPrefetchUrlProcess :: URI -> Maybe String -> IO LBS.ByteString
nixPrefetchUrlProcess uri maybeStoreName = do
  (ec, output) <-
    Process.readProcessStdout $
      Process.proc
        "nix-prefetch-url"
        (show uri : (["--type", "sha256"] ++ maybe [] (("--name" :) . pure) maybeStoreName))
  case ec of
    ExitSuccess -> return output
    ExitFailure i -> throwM (NixPrefetchUrlFailed i)

nixPrefetchGit :: URI -> Bool -> RIO NdtEnv Value
nixPrefetchGit uri fetchSubmodules = do
  action <- view nixPrefetchGitActionL
  liftIO $ action uri fetchSubmodules

nixPrefetchUrl :: URI -> Maybe String -> RIO NdtEnv LBS.ByteString
nixPrefetchUrl uri maybeStoreName = do
  action <- view nixPrefetchUrlActionL
  liftIO $ action uri maybeStoreName
