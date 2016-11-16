{-# LANGUAGE RecordWildCards #-}

module System.Hapistrano.PushRelease where

import           Control.Monad.Reader
import           Development.Shake
import           System.Hapistrano.Helpers


pushRelease :: IO ()
pushRelease = shake shakeOptions $ do  
  let config = Config
      { repository = "git@github.com:stackbuilders/hapistrano.git"
      , deployPath = "tmp"
      , revision = " 1b6e2c1010a6ac63dace0097de1298fea6dc2a14"   }
  action $ (`runReaderT` config) $ do
      createOrUpdateRepo
      stamp <- createRelease
      setReleaseRevison stamp
      deleteOldReleases

      
createOrUpdateRepo :: ReaderT Config Action ()
createOrUpdateRepo = do
  Config{..} <- ask
  let repoPath = deployPath ++ "/repo"
  exists <- lift $ doesDirectoryExist repoPath
  lift $ if exists
    then updateRepo repoPath
    else createRepo repository repoPath


createRelease :: ReaderT Config Action ReleasePath
createRelease = do
  deployPath <- asks deployPath
  release <- liftIO getTimestamp
  let releasePath = deployPath ++ "/releases/" ++ release
  lift (cmd "git clone " [deployPath ++ "/repo", releasePath] :: Action ())
  return release
  

deleteOldReleases :: ReaderT Config Action ()
deleteOldReleases = do
  delRels <- deletableRels
  mapM_ deleteRelease delRels


setReleaseRevison :: Release -> ReaderT Config Action Release
setReleaseRevison rel = do
  conf <- ask
  liftIO $ putStrLn "Setting revision in release path."
  -- Note that if we ran both actions like 'git fetch --all && git reset --hard'
  -- we'd get an error
  lift $ (cmd [Cwd $ releasesPath conf ++ rel] "git fetch --all" :: Action ())
  lift $ (cmd [Cwd $ releasesPath conf ++ rel] "git reset --hard" :: Action ())
  return rel
    
readCurrentLink :: ReaderT Config Action String
readCurrentLink = do
  conf <- ask
  let releasePath = (currentPath . deployPath) conf 
  lift (cmd $ "readlink " ++ "tmp/current" :: Action ()) 
  return releasePath
