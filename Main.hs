module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Data.Text (Text)

data Command
  = TrackDependency { githubUrl :: Text
                  , fetchSubmodules :: Bool
                  } deriving (Show)

commandParser :: Parser Command
commandParser = hsubparser ( command "track" (info trackOptions (progDesc "Track a new dependency")))

trackOptions = TrackDependency <$> argument str (metavar "GITHUB_URL")
                           <*> flag False True (long "fetch-submodules" <> help "Fetch submodules")

opts = info (commandParser <**> helper) (fullDesc <> header "Nix Dependency Tracker")

main :: IO ()
main = do
  options <- execParser opts
  print options
