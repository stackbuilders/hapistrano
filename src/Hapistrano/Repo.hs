{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Repo
  ( createOrUpdateRepo
  ) where

import           Development.Shake
import           Network.URL

import           Hapistrano.Types

createOrUpdateRepo :: URL -> RepoPath -> Action ()
createOrUpdateRepo repoUrl repoPath@RepoPath{..} = do
  exists <- doesDirectoryExist unRepoPath
  if exists
    then updateRepo repoPath
    else createRepo repoUrl repoPath

createRepo :: URL -> RepoPath -> Action ()
createRepo repoUrl RepoPath{..} =
  cmd "git clone --bare" [exportURL repoUrl, unRepoPath]

updateRepo :: RepoPath -> Action ()
updateRepo RepoPath{..} =
  cmd [Cwd unRepoPath] "git fetch origin +refs/heads/*:refs/heads/*"
