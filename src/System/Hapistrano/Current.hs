module System.Hapistrano.Current
  ( linkCurrent
  ) where

import           Development.Shake

-- FIXME: Convert paths to absolute paths
linkCurrent :: FilePath -> FilePath -> Action ()
linkCurrent = cmd "ln -sfT"
