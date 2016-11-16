{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Paths where

import           Development.Shake.FilePath

import           Hapistrano.Types

getCurrentPath :: DeployPath -> CurrentPath
getCurrentPath DeployPath{..} = CurrentPath $ unDeployPath </> "current"

getReleasesPath :: DeployPath -> ReleasesPath
getReleasesPath DeployPath{..} = ReleasesPath $ unDeployPath </> "releases"

getRepoPath :: DeployPath -> RepoPath
getRepoPath DeployPath{..} = RepoPath $ unDeployPath </> "repo"
