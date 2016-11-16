{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Repo where

import           Development.Shake
import           Network.URL

import           Hapistrano.Lock

createRepo :: URL -> FilePath -> Action ()
createRepo repoUrl repoPath =
  cmd "git clone --bare" (exportURL repoUrl) repoPath

updateRepo :: FilePath -> Action ()
updateRepo repoPath = cmd [Cwd repoPath] "git fetch --all"
