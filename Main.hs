module Main where

import qualified Data.ByteString.Lazy as LBS
import Control.Lens (to)
import Control.Lens.At (at, ix)
import Control.Lens.Operators
import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import Data.Aeson.Lens (_Object, _String)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI, parseAbsoluteURI)
import qualified Network.URI as URI
import Options.Applicative
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as Process
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, Indent(Spaces), Config(..))

data NdtEnv
  = NdtEnv
      { _ndtEnvSourcesFile :: FilePath
      }

data Command
  = TrackDependency Dependency
  | UpdateDependency Text
  deriving (Show)

data Dependency
  = GithubDependency
      { _ghDepDependencyKey :: Text,
        _ghDepGithubUrl :: URI,
        _ghDepFetchSubmodules :: Bool
      }
  deriving (Show)

data NdtError
  = NixPrefetchGitFailed ExitCode
  | NixPrefetchGitAesonDecodeError String
  deriving (Show)

commandParser :: Parser (NdtEnv, Command)
commandParser =
  (,)
    <$> ( NdtEnv
            <$> strOption (long "sources" <> short 's' <> metavar "SOURCES_FILE" <> value "sources.json" <> showDefault <> help "Read dependencies from SOURCES_FILE")
        )
    <*> ( hsubparser $
            command "track" (info trackOptions (progDesc "Track a new dependency"))
              <> command "update" (info updateOptions (progDesc "Update a tracked dependency"))
        )

trackOptions :: Parser Command
trackOptions =
  TrackDependency <$> hsubparser (command "github" (info trackGitHubOptions (progDesc "Track a GitHub repository")))

updateOptions :: Parser Command
updateOptions =
  UpdateDependency <$> argument str (metavar "DEPENDENCY")

trackGitHubOptions :: Parser Dependency
trackGitHubOptions =
  GithubDependency <$> (T.pack <$> argument str (metavar "NAME"))
    <*> argument uriReadM (metavar "GITHUB_URL")
    <*> flag False True (long "fetch-submodules" <> help "Fetch submodules")

opts :: ParserInfo (NdtEnv, Command)
opts = info (commandParser <**> helper) (fullDesc <> header "Nix Dependency Tracker")

main :: IO ()
main = do
  (env, options) <- execParser opts
  dispatch env options

dispatch :: NdtEnv -> Command -> IO ()
dispatch (NdtEnv sources) (TrackDependency (GithubDependency dependencyKey uri fsm)) = do
  eitherErrorJson <- nixPrefetchGit uri fsm
  case eitherErrorJson of
    Left e -> print e -- TODO display error
    Right json -> updateSources sources dependencyKey uri json
dispatch (NdtEnv sources) (UpdateDependency dependencyKey) = do
  decoded <- Aeson.eitherDecodeFileStrict @Value sources
  case decoded of
    Left _ -> undefined -- TODO: cant decode the file
    Right json -> do
      let dep = json ^? _Object . ix dependencyKey
      case dep of
        Nothing -> undefined -- TODO: no dependency found for key
        Just depValue -> do
          let Just uri = (depValue ^? _Object . ix "url" . _String . to T.unpack) >>= parseAbsoluteURI -- TODO: safe lookup
          eitherErrorJson <- nixPrefetchGit uri False -- TODO: fetchSubmodules from dep
          case eitherErrorJson of
            Left e -> print e -- TODO display error
            Right fetchedJson -> updateSources sources dependencyKey uri fetchedJson

updateSources :: FilePath -> Text -> URI -> Value -> IO ()
updateSources sources dependencyKey uri json = do
  let (owner, repo) = parseOwnerAndRepo uri
      json' =
        json & _Object . at "owner" ?~ Aeson.toJSON owner
          & _Object . at "repo" ?~ Aeson.toJSON repo
          & _Object . at "type" ?~ "github"
  withSources sources (_Object . at dependencyKey ?~ json')

parseOwnerAndRepo :: URI -> (String, String)
parseOwnerAndRepo uri = (owner, repo)
  where
    owner = takeWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri -- TODO: extract owner and repo
    repo = takeWhile (/= '/') . dropWhile (== '/') . dropWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri

nixPrefetchGit :: URI -> Bool -> IO (Either NdtError Value)
nixPrefetchGit uri fetchSubmodules = do
  (ec, stdout) <- Process.readProcessStdout $ Process.proc "nix-prefetch-git" (show uri : ["--fetch-submodules" | fetchSubmodules])
  return $ case ec of
    ExitSuccess -> case Aeson.eitherDecode' @Value stdout of
      Left e -> Left (NixPrefetchGitAesonDecodeError e)
      Right json -> Right json
    ExitFailure _ -> Left (NixPrefetchGitFailed ec)

uriReadM :: ReadM URI
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
          encoded = encodePretty' (defConfig { confIndent = Spaces 2 }) deps'
      LBS.writeFile fp encoded
