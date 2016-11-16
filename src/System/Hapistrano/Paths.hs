{-# LANGUAGE RecordWildCards #-}

module System.Hapistrano.Paths
  ( getCurrentPath
  , getReleasesPath
  , getRepoPath
  ) where

import           Development.Shake.FilePath

import           System.Hapistrano.NewTypes

getCurrentPath :: DeployTo -> CurrentPath
getCurrentPath DeployTo{..} = CurrentPath $ unDeployTo </> "current"

getReleasesPath :: DeployTo -> ReleasesPath
getReleasesPath DeployTo{..} = ReleasesPath $ unDeployTo </> "releases"

getRepoPath :: DeployTo -> RepoPath
getRepoPath DeployTo{..} = RepoPath $ unDeployTo </> "repo"
