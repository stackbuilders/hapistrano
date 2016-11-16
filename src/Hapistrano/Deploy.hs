{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Deploy where

import           Development.Shake
import           Development.Shake.FilePath

import           Hapistrano.Current
import           Hapistrano.Lock
import           Hapistrano.Paths
import           Hapistrano.Releases
import           Hapistrano.Repo
import           Hapistrano.Types

deploy :: Config -> IO ()
deploy Config{..} = do
  currentPath <- getCurrentPath configDeployPath
  releasesPath <- getReleasesPath configDeployPath
  repoPath <- getRepoPath configDeployPath
  release <- getRelease

  let releasePath = releasesPath </> release

  shakeArgs shakeOptions $ do
    wantLockFile releasePath

    withReleasePath releasesPath $ \f -> do
      needLockFile repoPath
      updateRepo repoPath
      createRelease repoPath f
      -- Link shared files
      -- Run build script
      removePreviousReleases releasesPath configKeepReleases
      linkCurrent f currentPath

    withLockFile repoPath $ createRepo configRepoUrl
