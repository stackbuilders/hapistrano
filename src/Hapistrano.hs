{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A module for easily creating reliable deploy processes for Haskell
-- applications.
module Hapistrano
       (
         Config(..)
       , initialState
       , runRC

       , activateRelease
       , runBuild
       , defaultSuccessHandler
       , defaultErrorHandler
       , pushRelease
       , restartServerCommand
       , rollback
       ) where

import Control.Lens (makeLenses, use, (^.), (.=))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.State (StateT, evalStateT, get)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either ( EitherT(..)
                                  , left
                                  , right
                                  , runEitherT
                                  , eitherT )
import Data.Char (isNumber)
import Data.List (intercalate, sortBy, sort, isInfixOf)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Exit (ExitCode(..))
import System.FilePath.Posix (joinPath, splitPath)
import System.IO (hPutStrLn, stderr)
import System.Locale (defaultTimeLocale)
import System.Process (readProcessWithExitCode)

-- | Config stuff that will be replaced by config file reading
data Config = Config { _deployPath :: String
                     , _host       :: String
                     , _repository :: String -- ^ The remote git repo
                     , _revision   :: String -- ^ A SHA1 or branch to release
                     , _buildScript    :: Maybe FilePath
                     , _restartCommand :: Maybe String
                     } deriving (Show)

makeLenses ''Config


data HapistranoState = HapistranoState { _config    :: Config
                                       , _timestamp :: Maybe String
                                       }
makeLenses ''HapistranoState

type Release = String

type RC a = StateT HapistranoState (EitherT (Int, Maybe String) IO) a

-- | Returns an initial state for the deploy.
initialState :: Config -> HapistranoState
initialState cfg = HapistranoState { _config    = cfg
                                   , _timestamp = Nothing
                                   }

-- | Given a pair of actions, one to perform in case of failure, and
-- one to perform in case of success, run an EitherT and get back a
-- monadic result.
runRC :: ((Int, Maybe String) -> IO a) -- ^ Error handler
      -> (a -> IO a)                   -- ^ Success handler
      -> HapistranoState               -- ^ Initial state
      -> RC a
      -> IO a
runRC errorHandler successHandler initState remoteCmd =
    eitherT errorHandler
            successHandler
            (evalStateT remoteCmd initState)

defaultErrorHandler :: (Int, Maybe String) -> IO ()
defaultErrorHandler _ = putStrLn "Deploy failed."

defaultSuccessHandler :: a -> IO ()
defaultSuccessHandler _ = putStrLn "Deploy completed successfully."



-- | Creates necessary directories for the hapistrano project. Should
-- only need to run the first time the project is deployed on a given
-- system.
setupDirs :: RC (Maybe String)
setupDirs = do
  pathName <- use $ config . deployPath
  remoteCommand $ "mkdir -p " ++ joinPath [pathName, "releases"]

remoteCommand :: String -- ^ The command to run remotely
              -> RC (Maybe String)
remoteCommand command = do
  server <- use $ config . host
  liftIO $ putStrLn $ "Going to execute " ++ command ++ " on host " ++ server
           ++ "."

  (code, stdout, err) <-
    liftIO $ readProcessWithExitCode "ssh" (server : words command) ""

  case code of
    ExitSuccess -> do
      liftIO $ putStrLn $ "Command '" ++ command ++
        "' was successful on host '" ++ server ++ "'."

      unless (null stdout) (liftIO $ putStrLn $ "Output:\n" ++ stdout)

      lift $ right $ maybeString stdout

    ExitFailure int -> do
      let maybeError = maybeString err
      liftIO $ printCommandError server command (int, maybeError)
      lift $ left (int, maybeError)

-- | Returns a timestamp in the default format for build directories.
currentTimestamp :: IO String
currentTimestamp = do
  curTime <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y%m%d%H%M%S" curTime

echoMessage :: String -> RC (Maybe String)
echoMessage msg = do
  liftIO $ putStrLn msg
  lift $ right Nothing

printCommandError :: String -> String -> (Int, Maybe String) -> IO ()
printCommandError server cmd (errCode, Nothing) =
  hPutStrLn stderr $ "Command " ++ " '" ++ cmd ++ "' failed on host '" ++
  server ++ "' with error code " ++ show errCode ++ " and no STDERR output."
printCommandError server cmd (errCode, Just errMsg) =
  hPutStrLn stderr $ "Command " ++ " '" ++ cmd ++ "' failed on host '" ++
  server ++ "' with error code " ++ show errCode ++ " and message '" ++
  errMsg ++ "'."

directoryExists :: FilePath -> RC (Maybe String)
directoryExists path =
  remoteCommand $ "ls " ++ path

-- | Returns the FilePath pointed to by the current symlink.
readCurrentLink :: RC (Maybe FilePath)
readCurrentLink = do
  conf <- use config
  remoteCommand $ "readlink " ++ currentPath conf

-- | Ensure that the initial bare repo exists in the repo directory. Idempotent.
ensureRepositoryPushed :: RC (Maybe String)
ensureRepositoryPushed = do
  conf <- use config
  res <- directoryExists $ cacheRepoPath conf

  case res of
    Nothing -> createCacheRepo
    Just _ -> lift $ right $ Just "Repo already existed"

-- | Returns a Just String or Nothing based on whether the input is null or
-- has contents.
maybeString :: String -> Maybe String
maybeString possibleString =
  if null possibleString then Nothing else Just possibleString

-- | Returns the full path of the folder containing all of the release builds.
releasesPath :: Config -> FilePath
releasesPath conf = joinPath [conf ^. deployPath, "releases"]

-- | Figures out the most recent release if possible, and sets the
-- StateT monad with the correct timestamp. This function is used
-- before rollbacks.
detectPrevious :: [String] -> RC (Maybe String)
detectPrevious rs = do
  let mostRecentRls = biggest rs
  case mostRecentRls of
    Nothing -> lift $ left (1, Just "No previous releases detected!")
    Just rls -> do
      timestamp .= mostRecentRls
      lift $ right $ Just rls

-- | Activates the previous detected release.
rollback :: RC (Maybe String)
rollback = previousReleases >>= detectPrevious >> activateRelease

-- | Clones the repository to the next releasePath timestamp. Makes a new
-- timestamp if one doesn't yet exist in the HapistranoState.
cloneToRelease :: RC (Maybe String)
cloneToRelease = do
  conf <- use config
  releaseTimestamp <- use timestamp

  rls <- case releaseTimestamp of
           Nothing -> do
             ts <- liftIO currentTimestamp
             timestamp .= Just ts
             return ts

           Just r -> return r

  remoteCommand $ "git clone " ++ cacheRepoPath conf ++ " " ++
    joinPath [ releasesPath conf, rls ]


-- | Returns the full path to the git repo used for cache purposes on the
-- target host filesystem.
cacheRepoPath :: Config -> FilePath
cacheRepoPath conf = joinPath [conf ^. deployPath, "repo"]

-- | Returns the full path to the current symlink.
currentPath :: Config -> FilePath
currentPath conf = joinPath [conf ^. deployPath, "current"]

-- | Take the release timestamp from the end of a filepath.
pathToRelease :: FilePath -> Release
pathToRelease = last . splitPath

-- | Returns a list of Strings representing the currently deployed releases.
releases :: RC [Release]
releases = do
  conf <- use config
  res  <- remoteCommand $ "find " ++ releasesPath conf ++ " -type d -maxdepth 1"

  case res of
    Nothing -> lift $ right []
    Just s ->
      lift $ right $ filter isReleaseString . map pathToRelease
      $ lines s

previousReleases :: RC [Release]
previousReleases = do
  rls <- releases
  currentRelease <- readCurrentLink

  case currentRelease of
    Nothing -> lift $ left (1, Just "Bad pointer from current link")
    Just c -> do
      let currentRel = (head . lines . pathToRelease) c
      return $ filter (< currentRel) rls

releasePath :: Config -> Release -> FilePath
releasePath conf rls = joinPath [releasesPath conf, rls]

-- | Given a list of release strings, takes the last four in the sequence.
-- Assumes a list of folders that has been determined to be a proper release
-- path.
oldReleases :: Config -> [Release] -> [FilePath]
oldReleases conf rs = map mergePath toDelete
  where sorted             = sortBy (flip compare) rs
        toDelete           = drop 4 sorted
        mergePath = releasePath conf

-- | Removes releases older than the last five to avoid filling up the target
-- host filesystem.
cleanReleases :: RC (Maybe String)
cleanReleases = do
  conf <- use config
  allReleases <- releases

  case allReleases of
    [] -> echoMessage "There are no old releases to prune."
    xs -> do
      let deletable = oldReleases conf xs

      remoteCommand $ "rm -rf -- " ++ foldr (\a b -> a ++ " " ++ b) ""
        deletable

-- | Returns a Bool indicating if the given String is in the proper release
-- format.
isReleaseString :: String -> Bool
isReleaseString s = all isNumber s && length s == 14

-- | Creates the git repository that is used on the target host for
-- cache purposes.
createCacheRepo :: RC (Maybe String)
createCacheRepo = do
  conf <- use config
  remoteCommand $ "git clone --bare " ++ conf ^. repository ++ " " ++
    cacheRepoPath conf

-- | Returns the full path of the symlink pointing to the current
-- release.
currentSymlinkPath :: Config -> FilePath
currentSymlinkPath conf = joinPath [conf ^. deployPath, "current"]

currentTempSymlinkPath :: Config -> FilePath
currentTempSymlinkPath conf = joinPath [conf ^. deployPath, "current_tmp"]

-- | Removes the current symlink in preparation for a new release being
-- activated.
removeCurrentSymlink :: RC (Maybe String)
removeCurrentSymlink = do
  conf <- use config
  remoteCommand $ "rm -rf " ++ currentSymlinkPath conf

-- | Determines whether the target host OS is Linux
remoteIsLinux :: RC Bool
remoteIsLinux = do
  st <- get
  res <- remoteCommand "uname"

  case res of
    Just output -> lift $ right $ "Linux" `isInfixOf` output
    _ -> lift $ left (1, Just "Unable to determine remote host type")

restartServerCommand :: RC (Maybe String)
restartServerCommand = do
  conf <- use config
  case conf ^. restartCommand of
    Nothing -> return $ Just "No command given for restart action."
    Just cmd -> remoteCommand cmd

runBuild :: RC (Maybe String)
runBuild = do
  conf <- use config
  case conf ^. buildScript of
    Nothing -> do
      liftIO $ putStrLn "No build script specified, skipping build step."
      return Nothing

    Just scr -> do
      fl <- liftIO $ readFile scr
      let commands = lines fl
      buildRelease commands

-- | Returns the best 'mv' command for a symlink given the target platform.
mvCommand ::
  Bool -- ^ Whether the target host is Linux
  -> String -- ^ The best mv command for a symlink on the platform
mvCommand True  = "mv -Tf"
mvCommand False = "mv -f"

-- | Creates a symlink to the directory indicated by the release timestamp.
symlinkCurrent :: RC (Maybe String)
symlinkCurrent = do
  conf <- use config
  releaseTimestamp <- use timestamp

  case releaseTimestamp of
    Nothing  -> lift $ left (1, Just "No releases to symlink!")
    Just rls -> do
      isLnx <- remoteIsLinux

      remoteCommand $ "ln -s " ++ rls ++ " " ++
        currentTempSymlinkPath conf ++
        " && " ++ mvCommand isLnx ++ " " ++
        currentTempSymlinkPath conf
        ++ " " ++ currentSymlinkPath conf

-- | Updates the git repo used as a cache in the target host filesystem.
updateCacheRepo :: RC (Maybe String)
updateCacheRepo = do
  conf <- use config
  remoteCommand $ intercalate " && "
    [ "cd " ++ cacheRepoPath conf
    , "git fetch origin +refs/heads/*:refs/heads/*" ]

-- | Sets the release to the correct revision by resetting the
-- head of the git repo.
setReleaseRevision :: RC (Maybe String)
setReleaseRevision = do
  conf <- use config
  releaseTimestamp <- use timestamp
  case releaseTimestamp of
    Nothing -> lift $ left (1, Just "No releases to symlink!")
    Just rls ->
      remoteCommand $ intercalate " && "
      [ "cd " ++ releasePath conf rls
      , "git fetch --all"
      , "git reset --hard " ++ conf ^. revision
      ]

-- | Returns a command that builds this application. Sets the context
-- of the build by switching to the release directory before running
-- the script.
buildRelease :: [String] -- ^ Commands to be run. List intercalated
                         -- with "&&" so that failure aborts the
                         -- sequence.
             -> RC (Maybe String)
buildRelease commands = do
  conf <- use config
  releaseTimestamp <- use timestamp
  case releaseTimestamp of
    Nothing -> lift $ left (1, Just "No releases to symlink!")
    Just rls -> do
      let cdCmd = "cd " ++ releasePath conf rls
      remoteCommand $ intercalate " && " $ cdCmd : commands

-- | A safe version of the `maximum` function in Data.List.
biggest :: Ord a => [a] -> Maybe a
biggest rls =
  case sortBy (flip compare) rls of
    []  -> Nothing
    r:_ -> Just r

-- | Does basic project setup for a project, including making sure
-- some directories exist, and pushing a new release directory with the
-- SHA1 or branch specified in the configuration.
pushRelease :: RC (Maybe String)
pushRelease = setupDirs >> ensureRepositoryPushed >> updateCacheRepo >>
              cleanReleases >> cloneToRelease >> setReleaseRevision

-- | Switches the current symlink to point to the release specified in
-- the configuration. Maybe used in either deploy or rollback cases.
activateRelease :: RC (Maybe String)
activateRelease = removeCurrentSymlink >> symlinkCurrent
