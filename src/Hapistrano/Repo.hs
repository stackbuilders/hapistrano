{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Repo where

import           Development.Shake
import           Network.URL

import           Hapistrano.Types

createRepo :: RepoUrl -> FilePath -> Action ()
createRepo RepoUrl{..} = cmd "git clone --bare" (exportURL unRepoUrl)

updateRepo :: FilePath -> Action ()
updateRepo repoPath = cmd [Cwd repoPath] "git fetch --all"
