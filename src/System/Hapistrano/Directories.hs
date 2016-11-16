{-# LANGUAGE RecordWildCards #-}

module System.Hapistrano.Directories where

import           Control.Monad.Reader
import           Development.Shake
import           Data.List (sort)
import           Data.Time.Clock 
import           Data.Time.Calendar (showGregorian)          
import           Control.Monad (mapM_)

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
          , revision = " 1b6e2c1010a6ac63dace0097de1298fea6dc2a14"
          }
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

createRepo :: String -> FilePath -> Action ()
createRepo repository repoPath =
  cmd "git clone --bare" [repository, repoPath]

updateRepo :: String -> Action ()
updateRepo repoPath =
  cmd [Cwd $ repoPath] "git fetch origin +refs/heads/*:refs/heads/*"

createRelease :: ReaderT Config Action ReleasePath
createRelease = do
  deployPath <- asks deployPath
  release <- liftIO getTimestamp
  let releasePath = deployPath ++ "/releases/" ++ release
  lift (cmd "git clone " [deployPath ++ "/repo", releasePath] :: Action ())
  return release
  
getTimestamp :: IO Release
getTimestamp = do
  (UTCTime date time) <- getCurrentTime
  return $ (filter (\s -> s /= '-') (showGregorian date)  ++  (init . show) (time * 1000000))

currentPath :: FilePath -> FilePath
currentPath depPath = depPath ++ "/releases" 

releasesPath :: Config -> FilePath
releasesPath conf = deployPath conf ++ "/releases/"  

releasePath :: Config -> Release -> FilePath
releasePath conf rel = releasesPath conf ++ "/" ++ rel 

deletableRels :: ReaderT Config Action [Release] 
deletableRels = do
  conf <- ask 
  rels <- lift $ getDirectoryDirs (releasesPath conf)
  let ordered = show <$> (sort $ (read <$> rels :: [Int]))
      size = length ordered 
  return $ if size > 5 then take (length ordered - 5) ordered else []

deleteRelease :: Release -> ReaderT Config Action ()
deleteRelease rel = ask >>= \conf -> lift $ cmd [Cwd $ releasesPath conf] ("rm -rf " ++ rel)  

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
