{-# LANGUAGE RecordWildCards #-}

module System.Hapistrano.Directories where

import           Control.Monad.Reader
import           Development.Shake

data Config = Config
  { deployPath :: FilePath
  , repository :: String
  } deriving (Eq, Show)

main :: IO ()
main = shake shakeOptions $ do
  let config =
        Config
          { repository = "git@github.com:stackbuilders/hapistrano.git"
          , deployPath = "tmp"
          }
  action $ do
    runReaderT createOrUpdateRepo config

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

createRelease :: ReaderT Config Action String
createRelease = do
  deployPath <- asks deployPath
  release <- liftIO getTimestamp
  let releasePath = deployPath ++ "/releases/" ++ release
  lift (cmd "git clone " [deployPath ++ "/repo", releasePath] :: Action ())
  return releasePath

getTimestamp :: IO String
getTimestamp = return "2016115"
