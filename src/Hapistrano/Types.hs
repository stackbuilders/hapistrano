module Hapistrano.Types where

import           Data.Default
import           Network.URL

data Config = Config
  { configDeployPath   :: DeployPath
  , configRepoUrl      :: URL
  , configKeepReleases :: KeepReleases
  } deriving (Eq, Show)

newtype CurrentPath = CurrentPath { unCurrentPath :: FilePath }
  deriving (Eq, Show)

newtype DeployPath = DeployPath { unDeployPath :: FilePath }
  deriving (Eq, Show)

newtype ReleasePath = ReleasePath { unReleasePath :: FilePath }
  deriving (Eq, Show)

newtype ReleasesPath = ReleasesPath { unReleasesPath :: FilePath }
  deriving (Eq, Show)

newtype RepoPath = RepoPath { unRepoPath :: FilePath }
  deriving (Eq, Show)

newtype KeepReleases = KeepReleases { unKeepReleases :: Int }
  deriving (Eq, Show)

instance Default KeepReleases where
  def = KeepReleases 5
