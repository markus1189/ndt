module Main where

import RIO
import RIO.Lens
import Lens.Micro.Platform ((?~))
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as LBS
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


class HasSourcesFile cfg where
  sourcesFileL :: Lens' cfg FilePath

class HasNixPrefetchAction cfg where
  nixPrefetchActionL :: Lens' cfg (URI -> Bool -> IO Value)

data NdtEnv
  = NdtEnv
      { _ndtEnvSourcesFile :: FilePath
      , _ndtEnvNixPrefetchGitAction :: URI -> Bool -> IO Value
      }

instance HasNixPrefetchAction NdtEnv where
  nixPrefetchActionL = lens _ndtEnvNixPrefetchGitAction (\x y -> x { _ndtEnvNixPrefetchGitAction = y})

instance HasSourcesFile NdtEnv where
  sourcesFileL = lens _ndtEnvSourcesFile (\x y -> x { _ndtEnvSourcesFile = y})

data NdtGlobal
  = NdtGlobal
      { _ndtGlobalSourcesFile :: FilePath
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

data NdtException
  = NixPrefetchGitFailed Int
  | NixPrefetchGitAesonDecodeError String
  | NoSuchDependency Text
  deriving (Typeable)

instance Exception NdtException

instance Show NdtException where
  show (NixPrefetchGitFailed ec) = "prefetching the git repository failed with exit code: " <> show ec
  show (NixPrefetchGitAesonDecodeError msg) = "could not parse the output of nix-prefetch-git: " <> msg

commandParser :: Parser (NdtGlobal, Command)
commandParser =
  (,)
    <$> ( NdtGlobal
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

opts :: ParserInfo (NdtGlobal, Command)
opts = info (commandParser <**> helper) (fullDesc <> header "Nix Dependency Tracker")

main :: IO ()
main = do
  (env@(NdtGlobal sourcesFp), options) <- execParser opts
  sourcesFilePresent <- doesFileExist sourcesFp
  -- TODO: fail if not there or create an empty file?
  let ndtEnv = NdtEnv sourcesFp nixPrefetchGitProcess
  runRIO ndtEnv $ dispatch options

dispatch :: Command -> RIO NdtEnv ()
dispatch (TrackDependency (GithubDependency dependencyKey uri fsm)) = do
  fetchedJson <- nixPrefetchGit uri fsm
  updateSources dependencyKey uri fetchedJson
dispatch (UpdateDependency dependencyKey) = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict @Value sources
  case decoded of
    Left msg -> throwM (NixPrefetchGitAesonDecodeError msg)
    Right json -> do
      let dep = json ^? _Object . ix dependencyKey
      case dep of
        Nothing -> throwM (NoSuchDependency dependencyKey)
        Just depValue -> do
          let Just uri = (depValue ^? _Object . ix "url" . _String . to T.unpack) >>= parseAbsoluteURI -- TODO: safe lookup
          fetchedJson <- nixPrefetchGit uri False -- TODO: fetchSubmodules from dep
          updateSources dependencyKey uri fetchedJson

updateSources :: Text -> URI -> Value -> RIO NdtEnv ()
updateSources dependencyKey uri json = do
  sources <- view sourcesFileL
  let (owner, repo) = parseOwnerAndRepo uri
      json' =
        json & _Object . at "owner" ?~ Aeson.toJSON owner
          & _Object . at "repo" ?~ Aeson.toJSON repo
          & _Object . at "type" ?~ "github"
  withSources (_Object . at dependencyKey ?~ json')

parseOwnerAndRepo :: URI -> (String, String)
parseOwnerAndRepo uri = (owner, repo)
  where
    owner = takeWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri -- TODO: extract owner and repo
    repo = takeWhile (/= '/') . dropWhile (== '/') . dropWhile (/= '/') . dropWhile (== '/') . URI.uriPath $ uri

nixPrefetchGitProcess :: URI -> Bool -> IO Value
nixPrefetchGitProcess uri fetchSubmodules = do
  (ec, stdout) <- Process.readProcessStdout $ Process.proc "nix-prefetch-git" (show uri : ["--fetch-submodules" | fetchSubmodules])
  case ec of
    ExitSuccess -> case Aeson.eitherDecode' @Value stdout of
      Left e -> throwM (NixPrefetchGitAesonDecodeError e)
      Right json -> return json
    ExitFailure i -> throwM (NixPrefetchGitFailed i)

nixPrefetchGit :: URI -> Bool -> RIO NdtEnv Value
nixPrefetchGit uri fetchSubmodules = do
  action <- view nixPrefetchActionL
  liftIO $ action uri fetchSubmodules

uriReadM :: ReadM URI
uriReadM = eitherReader parseAbsoluteURI'
  where
    parseAbsoluteURI' s = case parseAbsoluteURI s of
      Nothing -> Left $ "Not an absolute URI: '" <> s <> "'"
      Just u -> Right u

withSources :: (Value -> Value) -> RIO NdtEnv ()
withSources f = do
  sources <- view sourcesFileL
  decoded <- liftIO $ Aeson.eitherDecodeFileStrict sources
  case decoded of
    Left _ -> undefined -- TODO
    Right deps -> do
      let deps' = f deps
          encoded = encodePretty' (defConfig { confIndent = Spaces 2 }) deps'
      liftIO $ LBS.writeFile sources encoded
