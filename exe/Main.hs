{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Ndt
import Ndt.Types
import Ndt.Fetch

import Data.Coerce (coerce)
import RIO
import qualified RIO.ByteString
import System.Directory (doesFileExist)
import qualified Data.Text as T
import Network.URI (URI, parseAbsoluteURI)
import Options.Applicative
import qualified Data.FileEmbed as FE

newtype NdtGlobalOpts
  = NdtGlobalOpts
      { _ndtGlobalSourcesFile :: FilePath
      }

data Command
  = TrackDependency DependencyKey Dependency
  | UpdateDependency DependencyKey
  | PrintNixFile
  | Initialize
  | Refresh
  deriving (Eq, Show)

commandParser :: Parser (NdtGlobalOpts, Command)
commandParser =
  (,)
    <$> ( NdtGlobalOpts
            <$> strOption (long "sources" <> short 's' <> metavar "SOURCES_FILE" <> value "sources.json" <> showDefault <> help "Read dependencies from SOURCES_FILE")
        )
    <*> ( hsubparser $
            command "track" (info trackOptions (progDesc "Track a new dependency"))
            <> command "update" (info updateOptions (progDesc "Update a tracked dependency"))
            <> command "print" (info (pure PrintNixFile) (progDesc "Print a nix file to import sources"))
            <> command "init" (info (pure Initialize) (progDesc "Initialize a new ndt project"))
            <> command "refresh" (info (pure Refresh) (progDesc "Refresh all dependencies"))
        )

trackOptions :: Parser Command
trackOptions =
  TrackDependency <$> (coerce . T.pack <$> argument str (metavar "NAME"))
    <*> hsubparser (command "github" (info trackGitHubOptions (progDesc "Track a GitHub repository")) <> command "url" (info trackUrlOptions (progDesc "Track a URL as download")))

updateOptions :: Parser Command
updateOptions =
  UpdateDependency . coerce . T.pack <$> argument str (metavar "DEPENDENCY")

trackGitHubOptions :: Parser Dependency
trackGitHubOptions =
  GithubDependency <$> argument uriReadM (metavar "GITHUB_URL")
    <*> flag False True (long "fetch-submodules" <> help "Fetch submodules")
    <*> strOption (long "branch-name" <> short 'b' <> metavar "BRANCH_NAME" <> value "master" <> showDefault <> help "Branch name to check out into")

trackUrlOptions :: Parser Dependency
trackUrlOptions =
  UrlDependency <$> argument uriReadM (metavar "URL")
    <*> optional (strOption (long "store-name" <> metavar "NIX_STORE_NAME" <> help "Override the name of the file in the nix store"))

opts :: ParserInfo (NdtGlobalOpts, Command)
opts = info (commandParser <**> helper) (fullDesc <> header "Nix Dependency Tracker")

main :: IO ()
main = do
  (NdtGlobalOpts sourcesFp, options) <- execParser opts
  sourcesFilePresent <- doesFileExist sourcesFp
  when (not sourcesFilePresent && options /= Initialize) $
    throwM (UnreadableSources sourcesFp "no such file!")
  let ndtEnv = NdtEnv sourcesFp nixPrefetchGitProcess nixPrefetchUrlProcess
  runRIO ndtEnv $ dispatch options

dispatch :: Command -> RIO NdtEnv ()
dispatch (TrackDependency dk d) = trackDependency dk d
dispatch (UpdateDependency dk) = updateDependency dk
dispatch PrintNixFile = RIO.ByteString.putStr sourcesTemplateFile
dispatch Initialize = initialize
dispatch Refresh = updateAllDependencies

uriReadM :: ReadM URI
uriReadM = eitherReader parseAbsoluteURI'
  where
    parseAbsoluteURI' s = case parseAbsoluteURI s of
      Nothing -> Left $ "Not an absolute URI: '" <> s <> "'"
      Just u -> Right u

sourcesTemplateFile :: ByteString
sourcesTemplateFile = $(FE.embedFile "static/sources-template.nix")

sourcesNixFile :: ByteString
sourcesNixFile = $(FE.embedFile "static/init-template.nix")

initialize :: RIO NdtEnv ()
initialize = do
  sources <- view sourcesFileL
  unlessM (liftIO $ doesFileExist "sources.nix") $
    writeFileBinary "sources.nix" sourcesNixFile
  unlessM (liftIO $ doesFileExist sources) $
    writeFileBinary sources "{}"
