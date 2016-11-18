module Hapistrano.Shared where

import           Control.Monad
import           Control.Monad.IO.Class
import           Development.Shake
import           System.Directory
import           System.FilePath.Posix

linkSharedFiles :: FilePath -> FilePath-> [FilePath] -> Action ()
linkSharedFiles releasePath sharedPath filesToLink = do
  liftIO $  mapM_ createLinkDirectory release
  mapM_ createShareLink $ sharedAndLink share release
  where
    share = sharedLocations sharedPath filesToLink
    release = releaseLocations releasePath filesToLink

sharedAndLink :: [FilePath] -> [FilePath] -> [FilePath]
sharedAndLink shared release = map (\(s, r) -> s ++ " " ++ r) $
                                 zip shared release

sharedLocations :: FilePath -> [FilePath] -> [FilePath]
sharedLocations sharedPath = map (sharedPath </>)

releaseLocations :: FilePath ->  [FilePath] -> [FilePath]
releaseLocations releasePath = map (releasePath </>)

createShareLink :: FilePath -> Action ()
createShareLink = cmd "ln" "-snf"

createLinkDirectory :: FilePath -> IO ()
createLinkDirectory fileAndFolder = createDirectoryIfMissing True folderName
  where
    folderName = (joinPath . init . splitPath) fileAndFolder
