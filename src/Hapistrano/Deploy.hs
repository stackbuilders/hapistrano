{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Deploy where

import           Development.Shake
import           Development.Shake.FilePath

import           Hapistrano.Current
import           Hapistrano.Lock
import           Hapistrano.Paths
import           Hapistrano.Releases
import           Hapistrano.Repo
import           Hapistrano.Shared
import           Hapistrano.Types

deploy :: Config -> IO FilePath
deploy Config{..} = do
  currentPath <- getCurrentPath configDeployPath
  releasesPath <- getReleasesPath configDeployPath
  sharedPath <- getSharedPath configDeployPath
  repoPath <- getRepoPath configDeployPath
  release <- getRelease

  let releasePath = releasesPath </> release

  withShake configLogLevel $ do
    wantLockFile releasePath

    withReleasePath releasesPath $ \f -> do
      needLockFile repoPath
      updateRepo repoPath
      createRelease repoPath f configBranch
      linkSharedFiles releasePath sharedPath configLinkedFiles
      buildRelease releasePath configScriptPath
      removePreviousReleases releasesPath configKeepReleases
      linkCurrent f currentPath

    withLockFile repoPath $ createRepo configRepoUrl


  return releasePath

withShake :: LogLevel -> Rules () -> IO ()
withShake LogLevel{..} = shake shakeOptions { shakeVerbosity = unLogLevel }
