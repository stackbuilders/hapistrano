module System.Hapistrano.Main
  ( main
  ) where

import           Development.Shake
import           Development.Shake.FilePath

import           System.Hapistrano.Current
import           System.Hapistrano.Releases
import           System.Hapistrano.Repo
import           System.Hapistrano.Shared

main :: IO ()
main = do
  -- TODO: read from configuration
  let repository = "https://github.com/stackbuilders/hapistrano.git"
      projectPath = "/home/sestrella/code/stackbuilders/hapistrano/tmp"
      repoPath = projectPath </> "repo"
      releasesPath = projectPath </> "releases"
      currentPath = projectPath </> "current"
      sharedPath = projectPath </> "shared"
      keepReleases = 4
  shakeArgs shakeOptions $
    action $ do
      createOrUpdateRepo repository repoPath
      releasePath <- createRelease repoPath releasesPath
      linkSharedFiles sharedPath releasePath
      -- buildRelease releasePath scriptPath
      linkCurrent releasePath currentPath
      removePreviousReleases releasesPath keepReleases
