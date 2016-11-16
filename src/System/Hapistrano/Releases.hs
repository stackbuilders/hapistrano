{-# LANGUAGE RecordWildCards #-}

module System.Hapistrano.Releases
  ( createRelease
  , removePreviousReleases
  ) where

import           Control.Monad.Reader
import           Data.List
import           Data.Time
import           Development.Shake
import           Development.Shake.FilePath

import           System.Hapistrano.NewTypes

createRelease :: RepoPath -> ReleasesPath -> Action ReleasePath
createRelease RepoPath{..} ReleasesPath{..} = do
  release <- liftIO getRelease
  let releasePath = unReleasesPath </> release
  cmd "git clone " [unRepoPath, releasePath] :: Action ()
  return $ ReleasePath releasePath

getRelease :: IO String
getRelease = fmap(formatTime defaultTimeLocale format) getCurrentTime
  where
    format = "%Y%m%d%H%M%S"

removePreviousReleases :: ReleasesPath -> KeepReleases -> Action ()
removePreviousReleases releasesPath keepReleases =
  getPreviousReleases releasesPath keepReleases >>= cmd "rm -rf"

getPreviousReleases :: ReleasesPath -> KeepReleases -> Action [FilePath]
getPreviousReleases ReleasesPath{..} KeepReleases{..} = do
  fmap (drop unKeepReleases . reverse . sort . map (unReleasesPath </>)) $
    getDirectoryDirs unReleasesPath
