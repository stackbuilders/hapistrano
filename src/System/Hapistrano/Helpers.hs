module System.Hapistrano.Helpers where

import           Control.Monad.Reader
import           Development.Shake
import           Data.Text (Text)
import           Data.Time.Calendar (showGregorian)          
import           Control.Monad (mapM_)
import           Data.Time.Clock 
import           Data.List (sort)
import qualified Data.Text as T


data Config = Config
  { deployPath :: FilePath
  , repository :: String
  , revision :: String
  } deriving (Eq, Show)

type Release = String
type ReleasePath = String 


createRepo :: String -> FilePath -> Action ()
createRepo repository repoPath =
  cmd "git clone --bare" [repository, repoPath]


updateRepo :: String -> Action ()
updateRepo repoPath =
  cmd [Cwd $ repoPath] "git fetch origin +refs/heads/*:refs/heads/*"


deletableRels :: ReaderT Config Action [Release] 
deletableRels = do
  conf <- ask 
  rels <- lift $ getDirectoryDirs (releasesPath conf)
  let ordered = show <$> (sort $ (read <$> rels :: [Int]))
      size = length ordered 
  return $ if size > 5 then take (length ordered - 5) ordered else []


deleteRelease :: Release -> ReaderT Config Action ()
deleteRelease rel = ask >>= \conf -> lift $ cmd [Cwd $ releasesPath conf] ("rm -rf " ++ rel)  


currentPath :: FilePath -> FilePath
currentPath depPath = depPath ++ "/releases" 


releasesPath :: Config -> FilePath
releasesPath conf = deployPath conf ++ "/releases/"  


releasePath :: Config -> Release -> FilePath
releasePath conf rel = releasesPath conf ++ "/" ++ rel 


getTimestamp :: IO Release
getTimestamp = do
  (UTCTime date time) <- getCurrentTime
  return $ (filter (\s -> s /= '-') (showGregorian date)  ++  (init . show) (time * 1000000))


