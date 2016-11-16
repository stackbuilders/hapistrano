{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Paths where

import           Development.Shake.FilePath

import           Hapistrano.Types

getCurrentPath :: DeployTo -> CurrentPath
getCurrentPath DeployTo{..} = CurrentPath $ unDeployTo </> "current"

getReleasesPath :: DeployTo -> ReleasesPath
getReleasesPath DeployTo{..} = ReleasesPath $ unDeployTo </> "releases"

getRepoPath :: DeployTo -> RepoPath
getRepoPath DeployTo{..} = RepoPath $ unDeployTo </> "repo"
