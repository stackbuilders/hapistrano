module Hapistrano.Releases where

import           Data.List
import           Data.Time
import           Development.Shake
import           Development.Shake.FilePath

getRelease :: IO String
getRelease = fmap (formatTime defaultTimeLocale format) getCurrentTime
  where
    format = "%Y%m%d%H%M%S"

createRelease :: FilePath -> FilePath -> Action ()
createRelease repoPath releasePath = do
  cmd "rm -rf" releasePath :: Action ()
  cmd "git clone" repoPath releasePath

removePreviousReleases :: FilePath -> Int -> Action ()
removePreviousReleases releasesPath keepReleases =
  getPreviousReleases releasesPath keepReleases >>= cmd "rm -rf"

getPreviousReleases :: FilePath -> Int -> Action [FilePath]
getPreviousReleases releasesPath keepReleases =
  fmap (drop keepReleases . reverse . sort . map (releasesPath </>)) $
    getDirectoryDirs releasesPath
