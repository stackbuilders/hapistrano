{-# LANGUAGE RecordWildCards #-}

module System.NewHapistrano
  ( deploy
  ) where

import           Development.Shake

import           System.Hapistrano.Current
import           System.Hapistrano.NewTypes
import           System.Hapistrano.Paths
import           System.Hapistrano.Releases
import           System.Hapistrano.Repo

deploy :: Config -> Action ()
deploy Config{..} = do
  createOrUpdateRepo configRepoUrl repoPath
  releasePath <- createRelease repoPath releasesPath
  linkCurrent releasePath currentPath
  removePreviousReleases releasesPath configKeepReleases
  where
    currentPath = getCurrentPath configDeployTo
    releasesPath = getReleasesPath configDeployTo
    repoPath = getRepoPath configDeployTo
