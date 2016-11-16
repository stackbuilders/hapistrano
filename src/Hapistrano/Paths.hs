{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Paths where

import           Development.Shake.FilePath
import           System.Directory

getCurrentPath :: FilePath -> IO FilePath
getCurrentPath deployPath = makeAbsolute (deployPath </> "current")

getReleasesPath :: FilePath -> IO FilePath
getReleasesPath deployPath = makeAbsolute (deployPath </> "releases")

getRepoPath :: FilePath -> IO FilePath
getRepoPath deployPath = makeAbsolute (deployPath </> "repo")
