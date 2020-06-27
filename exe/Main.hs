module Main (main) where

import Ndt
import Ndt.Types
import Ndt.Fetch

import RIO
import System.Directory (doesFileExist)
import qualified Data.Text as T
import Network.URI (URI, parseAbsoluteURI)
import Options.Applicative

data NdtGlobalOpts
  = NdtGlobalOpts
      { _ndtGlobalSourcesFile :: FilePath
      }

data Command
  = TrackDependency Dependency
  | UpdateDependency Text
  deriving (Show)

commandParser :: Parser (NdtGlobalOpts, Command)
commandParser =
  (,)
    <$> ( NdtGlobalOpts
            <$> strOption (long "sources" <> short 's' <> metavar "SOURCES_FILE" <> value "sources.json" <> showDefault <> help "Read dependencies from SOURCES_FILE")
        )
    <*> ( hsubparser $
            command "track" (info trackOptions (progDesc "Track a new dependency"))
              <> command "update" (info updateOptions (progDesc "Update a tracked dependency"))
        )

trackOptions :: Parser Command
trackOptions =
  TrackDependency <$> hsubparser (command "github" (info trackGitHubOptions (progDesc "Track a GitHub repository")) <> command "url" (info trackUrlOptions (progDesc "Track a URL as download")))

updateOptions :: Parser Command
updateOptions =
  UpdateDependency <$> argument str (metavar "DEPENDENCY")

trackGitHubOptions :: Parser Dependency
trackGitHubOptions =
  GithubDependency <$> (T.pack <$> argument str (metavar "NAME"))
    <*> argument uriReadM (metavar "GITHUB_URL")
    <*> flag False True (long "fetch-submodules" <> help "Fetch submodules")

trackUrlOptions :: Parser Dependency
trackUrlOptions =
  UrlDependency <$> (T.pack <$> argument str (metavar "NAME"))
    <*> argument uriReadM (metavar "URL")
    <*> optional (strOption (long "store-name" <> metavar "NIX_STORE_NAME" <> help "Override the name of the file in the nix store"))

opts :: ParserInfo (NdtGlobalOpts, Command)
opts = info (commandParser <**> helper) (fullDesc <> header "Nix Dependency Tracker")

main :: IO ()
main = do
  (NdtGlobalOpts sourcesFp, options) <- execParser opts
  sourcesFilePresent <- doesFileExist sourcesFp
  -- TODO: fail if not there or create an empty file?
  let ndtEnv = NdtEnv sourcesFp nixPrefetchGitProcess nixPrefetchUrlProcess
  runRIO ndtEnv $ dispatch options -- TODO: catch exceptions and exitFailure

dispatch :: Command -> RIO NdtEnv ()
dispatch (TrackDependency d) = trackDependency d
dispatch (UpdateDependency dk) = updateDependency dk

uriReadM :: ReadM URI
uriReadM = eitherReader parseAbsoluteURI'
  where
    parseAbsoluteURI' s = case parseAbsoluteURI s of
      Nothing -> Left $ "Not an absolute URI: '" <> s <> "'"
      Just u -> Right u
