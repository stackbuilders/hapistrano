module Hapistrano.Types where

import           Network.URL

data Config = Config
  { configDeployPath   :: FilePath
  , configRepoUrl      :: URL
  , configKeepReleases :: Int
  } deriving (Eq, Show)
