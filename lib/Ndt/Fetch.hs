module Ndt.Fetch
  ( nixPrefetchGitProcess,
    nixPrefetchUrlProcess,
    fetchDependency
  )
where

import           Control.Monad.Catch (throwM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Data.Aeson ((.=), Value)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Lens.Micro.Platform (view, at, (&), (?~))
import           Ndt.Types
import           Network.URI (URI)
import qualified Network.URI as URI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.Exit (ExitCode(..))
import qualified System.Process.Typed as Process

fetchDependency :: (MonadReader env m, MonadIO m, HasNixPrefetchGitAction env, HasNixPrefetchUrlAction env) => Dependency -> m Value
fetchDependency (GithubDependency uri fetchSubmodules branchName) = do
  json <- nixPrefetchGit (NixPrefetchGitArgs uri fetchSubmodules branchName)
  let (owner, repo) = parseOwnerAndRepo uri
      json' =
        json & _Object . at "owner" ?~ Aeson.toJSON owner
          & _Object . at "repo" ?~ Aeson.toJSON repo
          & _Object . at "type" ?~ "github"
          & _Object . at "branch" ?~ Aeson.toJSON branchName
  pure json'
fetchDependency (UrlDependency uri storeName) = do
  sha256 <- nixPrefetchUrl (NixPrefetchUrlArgs uri storeName)
  let json =
        Aeson.object $
          [ "url" .= show uri,
            "type" .= ("url" :: Text),
            "sha256" .= T.dropWhileEnd (== '\n') (T.decodeUtf8 (LBS.toStrict sha256))
          ]
            ++ maybe [] (pure . ("name" .=)) storeName
  pure json

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

parseOwnerAndRepo :: URI -> (String, String)
parseOwnerAndRepo uri = (owner, repo)
  where
    owner = takeWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri
    repo = takeWhile (/= '/') . dropWhile (== '/') . dropWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri
