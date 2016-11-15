{-# LANGUAGE RecordWildCards #-}

module System.Hapistrano.Directories where

import           Control.Monad.Reader
import           Development.Shake
import           Data.List (intercalate)
import qualified Data.Time as TM



data Config = Config
  { deployPath :: FilePath
  , repository :: String
  , revision :: String
  } deriving (Eq, Show)

type Release = String
type ReleasePath = String 


main' :: IO ()
main' = shake shakeOptions $ do
  let config =
        Config
          { repository = "git@github.com:stackbuilders/hapistrano.git"
          , deployPath = "tmp"
          , revision = " origin/staging"
          }
  action $ (`runReaderT` config) $ do
    createOrUpdateRepo
    stamp <- createRelease
    setReleaseRevison stamp 

    
createOrUpdateRepo :: ReaderT Config Action ()
createOrUpdateRepo = do
  Config{..} <- ask
  let repoPath = deployPath ++ "/repo"
  exists <- lift $ doesDirectoryExist repoPath
  lift $ if exists
    then updateRepo repoPath
    else createRepo repository repoPath

createRepo :: String -> FilePath -> Action ()
createRepo repository repoPath =
  cmd "git clone --bare" [repository, repoPath]

updateRepo :: String -> Action ()
updateRepo repoPath =
  cmd [Cwd repoPath] "git fetch origin +refs/heads/*:refs/heads/*"

createRelease :: ReaderT Config Action ReleasePath
createRelease = do
  deployPath <- asks deployPath
  release <- liftIO getTimestamp
  let releasePath = deployPath ++ "/releases/" ++ release
  lift (cmd "git clone " [deployPath ++ "/repo", releasePath] :: Action ())
  return release --releasePath

getTimestamp :: IO Release
getTimestamp = do
  (TM.UTCTime _ time) <- TM.getCurrentTime
  return $ show (time * 1000000)

currentPath :: FilePath -> FilePath
currentPath depPath = depPath ++ "/releases" 

releasesPath :: Config -> FilePath
releasesPath conf = deployPath conf ++ "/releases"  

releasePath :: Config -> Release -> FilePath
releasePath conf rel = releasesPath conf ++ "/" ++ rel 


setReleaseRevison :: Release -> ReaderT Config Action Release
setReleaseRevison rel = do
  conf <- ask
  liftIO $ putStrLn "Setting revision in release path."
  lift $ (cmd $ intercalate " && " [ "cd " ++ "releases/" ++ rel
                         , "git fetch --all"
                         , "git reset --hard" ++ revision conf
                         ] :: Action ())
  return rel  

  
readCurrentLink :: ReaderT Config Action String
readCurrentLink = do
  conf <- ask
  let releasePath = (currentPath . deployPath) conf 
  lift (cmd $ "readlink " ++ "tmp/current" :: Action ()) 
  return releasePath
