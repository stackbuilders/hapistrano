module System.Hapistrano.Shared where

import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath

linkSharedFiles :: FilePath -> FilePath -> Action ()
linkSharedFiles sharedPath releasePath = do
  exists <- doesDirectoryExist sharedPath
  when exists $ do
    files <- getDirectoryFiles sharedPath ["//*"]
    mapM_ (\f -> cmd "ln -s" (sharedPath </> f) (releasePath </> f) :: Action ()) files
