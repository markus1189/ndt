module Tests.Fixtures (ndtSource, ndtDefaultNixUrlSource, sources) where

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import Ndt.Types (Sources(..))
import qualified Data.HashMap.Strict as HM

ndtSource :: Value
ndtSource = Aeson.object [ "deepClone" .= False
                         , "path" .= ("/nix/store/8fdri4a53dw3axxp27q645dk1r72gy75-ndt" :: String)
                         , "url" .= ("https://github.com/markus1189/ndt" :: String)
                         , "leaveDotGit" .= False
                         , "owner" .= ("markus1189" :: String)
                         , "fetchSubmodules" .= False
                         , "date" .= ("2020-07-03T14:09:57+02:00" :: String)
                         , "branch" .= ("master" :: String)
                         , "repo" .= ("ndt" :: String)
                         , "type" .= ("github" :: String)
                         , "sha256" .= ("13k6098vkal5b0rp7psw0i2myamkwqwizjh6yjapd8kghbfx249w" :: String)
                         , "rev" .= ("b77d5f96fc24bc3368fd871a7822974dfb3ebedc" :: String)
                         ]

ndtDefaultNixUrlSource :: Value
ndtDefaultNixUrlSource = Aeson.object [ "url" .= ("https://raw.githubusercontent.com/markus1189/ndt/master/default.nix" :: String)
                                      , "name" .= ("ndt-default-nix" :: String)
                                      , "type" .= ("url" :: String)
                                      , "sha256" .= ("1y3snx762sknm4m913m3m67rfqnm88xs3bf6mms669cclw2513dy" :: String)
                                      ]

sources :: Sources
sources = Sources $ HM.fromList [ ("ndt", ndtSource)
                                , ("ndt-default-nix", ndtDefaultNixUrlSource)
                                ]
