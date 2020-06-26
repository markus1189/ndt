module Main where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Options.Applicative
import Network.URI (parseAbsoluteURI, URI)

data Command
  = TrackDependency Dependency
  deriving (Show)

data Dependency
  = GithubDependency
      { githubUrl :: URI,
        fetchSubmodules :: Bool
      }
  deriving (Show)

commandParser :: Parser Command
commandParser = hsubparser (command "track" (info trackOptions (progDesc "Track a new dependency")))

trackOptions =
  TrackDependency <$> hsubparser (command "github" (info trackGitHubOptions (progDesc "Track a GitHub repository")))

trackGitHubOptions =
  GithubDependency <$> argument uriReadM (metavar "GITHUB_URL")
                   <*> flag False True (long "fetch-submodules" <> help "Fetch submodules")

opts = info (commandParser <**> helper) (fullDesc <> header "Nix Dependency Tracker")

main :: IO ()
main = do
  options <- execParser opts
  dispatch options


dispatch (TrackDependency (GithubDependency u _)) = print u


uriReadM = eitherReader parseAbsoluteURI'
  where parseAbsoluteURI' s = case parseAbsoluteURI s of
          Nothing -> Left $ "Not an absolute URI: '" <> s <> "'"
          Just u -> Right u
