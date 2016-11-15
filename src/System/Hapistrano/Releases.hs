module System.Hapistrano.Releases
  ( createRelease
  , buildRelease
  , removePreviousReleases
  ) where

import           Control.Monad.Reader
import           Data.List
import           Data.Time
import           Development.Shake

createRelease :: FilePath -> FilePath -> Action String
createRelease repoPath releasesPath = do
  release <- liftIO getRelease
  let releasePath = releasesPath <//> release
  cmd "git clone " [repoPath, releasePath] :: Action ()
  return releasePath

getRelease :: IO String
getRelease = fmap(formatTime defaultTimeLocale format) getCurrentTime
  where
    format = "%Y%m%d%H%M%S"

buildRelease :: FilePath -> FilePath -> Action ()
buildRelease releasePath scriptPath = do
  cmd [Cwd releasePath] "source" scriptPath

removePreviousReleases :: FilePath -> Int -> Action ()
removePreviousReleases releasesPath keepReleases =
  getPreviousReleases releasesPath keepReleases >>= cmd "rm -rf"

getPreviousReleases :: FilePath -> Int -> Action [FilePath]
getPreviousReleases releasesPath keepReleases = do
  fmap (drop keepReleases . reverse . sort . map (releasesPath <//>)) $
    getDirectoryDirs releasesPath
