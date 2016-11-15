module System.Hapistrano.Main
  ( main
  ) where

import           Development.Shake

import           System.Hapistrano.Current
import           System.Hapistrano.Releases
import           System.Hapistrano.Repo

main :: IO ()
main = do
  let repository = "https://github.com/stackbuilders/hapistrano.git"
      repoPath = "tmp/repo"
      releasesPath = "tmp/releases"
      currentPath = "tmp/current"
      scriptPath = "build.sh"
      keepReleases = 4
  shakeArgs shakeOptions $
    action $ do
      createOrUpdateRepo repository repoPath
      releasePath <- createRelease repoPath releasesPath
      -- buildRelease releasePath scriptPath
      symlinkCurrent releasePath currentPath
      removePreviousReleases releasesPath keepReleases
