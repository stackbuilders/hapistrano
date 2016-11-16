module Hapistrano.Lock where

import           Development.Shake
import           Development.Shake.FilePath

wantLockFile :: FilePath -> Rules ()
wantLockFile dir = want [lockFile dir]

-- TODO: Rename foo
withLockFile :: FilePath -> (FilePath -> Action ()) -> Rules ()
withLockFile dir f = lockFile dir %> \foo -> do
  f $ takeDirectory foo
  cmd "touch" foo

needLockFile :: FilePath -> Action ()
needLockFile dir = need [lockFile dir]

lockFile :: FilePath -> FilePath
lockFile dir = dir </> ".lock"
