{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Internal where

import           Development.Shake
import           Development.Shake.FilePath

import           Hapistrano.Current
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
    want [joinPath [releasePath, ".git", "HEAD"]]

    joinPath [releasePath, ".git", "HEAD"] %> \_ -> do
      need [joinPath [repoPath, "HEAD"]]
      updateRepo repoPath
      createRelease repoPath releasePath
      removePreviousReleases releasesPath configKeepReleases
      linkCurrent releasePath currentPath

    joinPath [repoPath, "HEAD"] %> \_ ->
      createRepo configRepoUrl repoPath
