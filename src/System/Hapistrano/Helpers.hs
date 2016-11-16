module System.Hapistrano.Helpers where

import           Control.Monad.Reader
import           Development.Shake
import           Data.Text (Text)
import           Data.Time.Calendar (showGregorian)          
import           Control.Monad (mapM_)
import           Data.Time.Clock 
import           Data.List (sort)
import qualified Data.Text as T


-- This data type represents the configuration provided from
-- the Environment of the local machine.
data Config = Config
  { deployPath :: FilePath
  , repository :: String
  , revision :: String
  } deriving (Eq, Show)

type Release = String
type ReleasePath = String 


-- Clones (-- bare) a repository into a given path.
createRepo :: String -> FilePath -> Action ()
createRepo repository repoPath =
  cmd "git clone --bare" [repository, repoPath]


-- Fetches a the last commit of the repository in the repository cache.
updateRepo :: String -> Action ()
updateRepo repoPath =
  cmd [Cwd $ repoPath] "git fetch origin +refs/heads/*:refs/heads/*"


-- Determines which are the releases which are older than the five most
-- recent. This definition is not optimal but works for demonstration
-- purposes.
deletableRels :: ReaderT Config Action [Release] 
deletableRels = do
  conf <- ask 
  rels <- lift $ getDirectoryDirs (releasesPath conf)
  let ordered = show <$> (sort $ (read <$> rels :: [Int]))
      size = length ordered 
  return $ if size > 5 then take (length ordered - 5) ordered else []


-- Given a timeStamp of a release, delete it.
deleteRelease :: Release -> ReaderT Config Action ()
deleteRelease rel = ask >>= \conf -> lift $ cmd [Cwd $ releasesPath conf] ("rm -rf " ++ rel)  


-- Determines the /releases path from config.
releasesPath :: Config -> FilePath
releasesPath conf = deployPath conf ++ "/releases/"  


-- Determines the path of an specific release. 
releasePath :: Config -> Release -> FilePath
releasePath conf rel = releasesPath conf ++ "/" ++ rel 



-- creates a time-stamp out of the current time.
getTimestamp :: IO Release
getTimestamp = do
  (UTCTime date time) <- getCurrentTime
  return $ (filter (\s -> s /= '-') (showGregorian date)  ++  (init . show) (time * 1000000))


