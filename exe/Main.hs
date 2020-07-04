{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Ndt
import           Ndt.Types
import           Ndt.Fetch
import           Ndt.Sources (listDependencies)

import           Data.Aeson (Value)
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce (coerce)
import qualified Data.FileEmbed as FE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.URI (URI, parseAbsoluteURI)
import           Options.Applicative
import           RIO
import qualified RIO.ByteString
import           System.Directory (doesFileExist)
import           System.FilePath ((-<.>))

data NdtEnv
  = NdtEnv
      { _ndtEnvSourcesFile :: FilePath,
        _ndtEnvNixPrefetchGitAction :: NixPrefetchGitArgs -> IO Value,
        _ndtEnvNixPrefetchUrlAction :: NixPrefetchUrlArgs -> IO LBS.ByteString,
        _ndtEnvLogFunc :: LogFunc
      }

instance HasLogFunc NdtEnv where
  logFuncL = lens _ndtEnvLogFunc (\x y -> x { _ndtEnvLogFunc = y })

instance HasSourcesFile NdtEnv where
  sourcesFileL = lens _ndtEnvSourcesFile (\x y -> x {_ndtEnvSourcesFile = y})

instance HasNixPrefetchGitAction NdtEnv where
  nixPrefetchGitActionL = lens _ndtEnvNixPrefetchGitAction (\x y -> x {_ndtEnvNixPrefetchGitAction = y})

instance HasNixPrefetchUrlAction NdtEnv where
  nixPrefetchUrlActionL = lens _ndtEnvNixPrefetchUrlAction (\x y -> x {_ndtEnvNixPrefetchUrlAction = y})

newtype NdtGlobalOpts
  = NdtGlobalOpts
      { _ndtGlobalSourcesFile :: FilePath
      }

data Command
  = TrackDependency DependencyKey Dependency
  | UpdateDependency DependencyKey
  | DeleteDependency DependencyKey
  | PrintNixFile
  | Initialize
  | UpdateAll
  | ListDependencies
  deriving (Eq, Show)

commandParser :: Parser (NdtGlobalOpts, Command)
commandParser =
  (,)
    <$> ( NdtGlobalOpts
            <$> strOption (long "sources" <> short 's' <> metavar "SOURCES_FILE" <> value "sources.json" <> showDefault <> help "Read dependencies from SOURCES_FILE")
        )
    <*> ( hsubparser $ command "track" (info trackOptions (progDesc "Track a new dependency"))
          <> command "update" (info updateOptions (progDesc "Update a tracked dependency"))
          <> command "print" (info (pure PrintNixFile) (progDesc "Print a nix file to import sources"))
          <> command "init" (info (pure Initialize) (progDesc "Initialize a new ndt project"))
          <> command "update-all" (info (pure UpdateAll) (progDesc "Update all all dependencies"))
          <> command "del" (info deleteOptions (progDesc "Delete a dependency from the sources"))
          <> command "ls" (info (pure ListDependencies) (progDesc "List all known dependencies"))
        )

deleteOptions :: Parser Command
deleteOptions = DeleteDependency <$> argument dkM (metavar "DEPENDENCY")

trackOptions :: Parser Command
trackOptions =
  TrackDependency <$> argument dkM (metavar "NAME")
    <*> hsubparser (command "github" (info trackGitHubOptions (progDesc "Track a GitHub repository")) <> command "url" (info trackUrlOptions (progDesc "Track a URL as download")))

updateOptions :: Parser Command
updateOptions = UpdateDependency <$> argument dkM (metavar "DEPENDENCY")

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
  logOptions <- logOptionsHandle stderr False
  withLogFunc logOptions $ \lf -> do
    let ndtEnv = NdtEnv sourcesFp nixPrefetchGitProcess nixPrefetchUrlProcess lf
    runRIO ndtEnv $ dispatch options

dispatch :: Command -> RIO NdtEnv ()
dispatch (TrackDependency dk d) = trackDependency dk d
dispatch (UpdateDependency dk) = updateDependency dk
dispatch (DeleteDependency dk) = do
  logInfo $ "Deleting: '" <> display (coerce dk :: Text) <> "'"
  deleteDependency dk
dispatch PrintNixFile = RIO.ByteString.putStr sourcesTemplateFile
dispatch Initialize = initialize
dispatch UpdateAll = updateAllDependencies
dispatch ListDependencies = do
  dks <- listDependencies
  if null dks
    then logInfo "You don't have any dependencies yet!"
    else for_ dks (liftIO . TIO.putStrLn . coerce)

uriReadM :: ReadM URI
uriReadM = eitherReader parseAbsoluteURI'
  where
    parseAbsoluteURI' s = case parseAbsoluteURI s of
      Nothing -> Left $ "Not an absolute URI: '" <> s <> "'"
      Just u -> Right u

dkM :: ReadM DependencyKey
dkM = maybeReader (Just . coerce . T.pack)

sourcesTemplateFile :: ByteString
sourcesTemplateFile = $(FE.embedFile "static/sources-template.nix")

sourcesNixFile :: ByteString
sourcesNixFile = $(FE.embedFile "static/init-template.nix")

initialize :: RIO NdtEnv ()
initialize = do
  sources <- view sourcesFileL
  let sourcesNix = sources -<.> "nix"
  unlessM (liftIO $ doesFileExist sourcesNix) $ do
    logInfo $ "Initializing file: " <> display (T.pack sourcesNix)
    writeFileBinary sourcesNix sourcesNixFile
  unlessM (liftIO $ doesFileExist sources) $ do
    logInfo $ "Initializing file: " <> display (T.pack sources)
    writeFileBinary sources "{}"
