module System.Hapistrano.Current where

import           Development.Shake

-- FIXME: Convert paths to absolute paths
symlinkCurrent :: FilePath -> FilePath -> Action ()
symlinkCurrent releasePath currentPath =
  cmd "ln -s" releasePath currentPath
