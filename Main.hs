module Main where

import Control.Lens (to)
import Control.Lens.At (at, ix)
import Control.Lens.Operators
import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import Data.Aeson.Lens (_Object, _String)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI, parseAbsoluteURI)
import qualified Network.URI as URI
import Options.Applicative
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as Process

data Command
  = TrackDependency Dependency
  | UpdateDependency Text
  deriving (Show)

data Dependency
  = GithubDependency
      { dependencyKey :: Text,
        githubUrl :: URI,
        fetchSubmodules :: Bool
      }
  deriving (Show)

data NdtError
  = NixPrefetchGitFailed ExitCode
  | NixPrefetchGitAesonDecodeError String
  deriving (Show)

commandParser :: Parser Command
commandParser =
  hsubparser $
    command "track" (info trackOptions (progDesc "Track a new dependency"))
      <> command "update" (info updateOptions (progDesc "Update a tracked dependency"))

trackOptions =
  TrackDependency <$> hsubparser (command "github" (info trackGitHubOptions (progDesc "Track a GitHub repository")))

updateOptions =
  UpdateDependency <$> argument str (metavar "DEPENDENCY")

trackGitHubOptions =
  GithubDependency <$> (T.pack <$> argument str (metavar "NAME"))
    <*> argument uriReadM (metavar "GITHUB_URL")
    <*> flag False True (long "fetch-submodules" <> help "Fetch submodules")

opts = info (commandParser <**> helper) (fullDesc <> header "Nix Dependency Tracker")

main :: IO ()
main = do
  options <- execParser opts
  dispatch options

dispatch :: Command -> IO ()
dispatch (TrackDependency (GithubDependency dependencyKey uri fsm)) = do
  eitherErrorJson <- nixPrefetchGit uri fsm
  case eitherErrorJson of
    Left e -> print e -- TODO display error
    Right json -> updateSources dependencyKey uri json
dispatch (UpdateDependency dependencyKey) = do
  decoded <- Aeson.eitherDecodeFileStrict @Value sourcesFilePath
  case decoded of
    Left e -> undefined -- TODO: cant decode the file
    Right json -> do
      let dep = json ^? _Object . ix dependencyKey
      case dep of
        Nothing -> undefined -- TODO: no dependency found for key
        Just value -> do
          let Just uri = (value ^? _Object . ix "url" . _String . to T.unpack) >>= parseAbsoluteURI -- TODO: safe lookup
          eitherErrorJson <- nixPrefetchGit uri False -- TODO: fetchSubmodules from dep
          case eitherErrorJson of
            Left e -> print e -- TODO display error
            Right json -> updateSources dependencyKey uri json

updateSources dependencyKey uri json = do
  let (owner, repo) = parseOwnerAndRepo uri
      json' =
        json & _Object . at "owner" ?~ Aeson.toJSON owner
          & _Object . at "repo" ?~ Aeson.toJSON repo
          & _Object . at "type" ?~ "github"
  withSources sourcesFilePath (_Object . at dependencyKey ?~ json')

parseOwnerAndRepo :: URI -> (String, String)
parseOwnerAndRepo uri = (owner, repo)
  where
    owner = takeWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri -- TODO: extract owner and repo
    repo = takeWhile (/= '/') . dropWhile (== '/') . dropWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri

nixPrefetchGit uri fetchSubmodules = do
  (ec, stdout) <- Process.readProcessStdout $ Process.proc "nix-prefetch-git" [show uri]
  return $ case ec of
    ExitSuccess -> case Aeson.eitherDecode' @Value stdout of
      Left e -> Left (NixPrefetchGitAesonDecodeError e)
      Right json -> Right json
    ExitFailure _ -> Left (NixPrefetchGitFailed ec)

uriReadM = eitherReader parseAbsoluteURI'
  where
    parseAbsoluteURI' s = case parseAbsoluteURI s of
      Nothing -> Left $ "Not an absolute URI: '" <> s <> "'"
      Just u -> Right u

withSources :: FilePath -> (Value -> Value) -> IO ()
withSources fp f = do
  decoded <- Aeson.eitherDecodeFileStrict fp
  case decoded of
    Left _ -> undefined -- TODO
    Right deps -> do
      let deps' = f deps
      Aeson.encodeFile fp deps'

sourcesFilePath = "sources.json"
