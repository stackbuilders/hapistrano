{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Releases where

import           Data.List
import           Data.Time
import           Development.Shake
import           Development.Shake.FilePath

import           Hapistrano.Lock
import           Hapistrano.Types

getRelease :: IO String
getRelease = fmap (formatTime defaultTimeLocale format) getCurrentTime
  where
    format = "%Y%m%d%H%M%S"

withReleasePath :: FilePath -> (FilePath -> Action ()) -> Rules ()
withReleasePath releasesPath = withLockFile (releasesPath </> "*")

createRelease :: FilePath -> FilePath -> Branch -> Action ()
createRelease repoPath releasePath Branch{..} =
  cmd "git clone -b" unBranch repoPath releasePath

buildRelease :: FilePath -> FilePath -> Action ()
buildRelease releasePath scriptPath = do
  need [releasePath </> scriptPath]
  cmd [Cwd releasePath] "source"

removePreviousReleases :: FilePath -> KeepReleases -> Action ()
removePreviousReleases releasesPath keepReleases =
  getPreviousReleases releasesPath keepReleases >>= cmd "rm -rf"

getPreviousReleases :: FilePath -> KeepReleases -> Action [FilePath]
getPreviousReleases releasesPath KeepReleases{..} =
  fmap (drop unKeepReleases . reverse . sort . map (releasesPath </>)) $
    getDirectoryDirs releasesPath
