module System.Hapistrano.NewTypes where

import           Data.Default
import           Network.URL

data Config = Config
  { configDeployTo     :: DeployTo
  , configRepoUrl      :: URL
  , configKeepReleases :: KeepReleases
  } deriving (Eq, Show)

newtype DeployTo = DeployTo { unDeployTo :: FilePath }
  deriving (Eq, Show)

newtype KeepReleases = KeepReleases { unKeepReleases :: Int }
  deriving (Eq, Show)

instance Default KeepReleases where
  def = KeepReleases 5

newtype CurrentPath = CurrentPath { unCurrentPath :: FilePath }
  deriving (Eq, Show)

newtype ReleasePath = ReleasePath { unReleasePath :: FilePath }
  deriving (Eq, Show)

newtype ReleasesPath = ReleasesPath { unReleasesPath :: FilePath }
  deriving (Eq, Show)

newtype RepoPath = RepoPath { unRepoPath :: FilePath }
  deriving (Eq, Show)
