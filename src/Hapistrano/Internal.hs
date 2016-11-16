{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Internal where

import           Development.Shake

import           Hapistrano.Current
import           Hapistrano.Paths
import           Hapistrano.Releases
import           Hapistrano.Repo
import           Hapistrano.Types

deploy :: Config -> IO ()
deploy Config{..} = shakeArgs shakeOptions $ action $ do
  createOrUpdateRepo configRepoUrl repoPath
  releasePath <- createRelease repoPath releasesPath
  linkCurrent releasePath currentPath
  removePreviousReleases releasesPath configKeepReleases
  where
    currentPath = getCurrentPath configDeployTo
    releasesPath = getReleasesPath configDeployTo
    repoPath = getRepoPath configDeployTo
