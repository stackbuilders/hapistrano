{-# LANGUAGE OverloadedStrings #-}

-- | A module for easily creating reliable deploy processes for Haskell
-- applications.
module System.Hapistrano
       ( Config(..)

       , activateRelease
       , currentPath
       , defaultSuccessHandler
       , defaultErrorHandler
       , directoryExists
       , initialState
       , isReleaseString
       , pathToRelease
       , pushRelease
       , readCurrentLink
       , restartServerCommand
       , rollback
       , runRC
       , runBuild

       ) where

import System.Hapistrano.Types (
  Config(..), HapistranoState(..), RC, Release, ReleaseFormat(..))

import Control.Monad (unless, void)
import System.Exit (ExitCode(..), exitWith)

import Control.Monad.State.Lazy (gets, put)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.State (evalStateT, get)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either ( left
                                  , right
                                  , eitherT )

import Data.Char (isNumber)
import Data.List (intercalate, sortBy, isInfixOf)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime)
import System.FilePath.Posix (joinPath, splitPath)
import System.IO (hPutStrLn, stderr)
import System.Locale (defaultTimeLocale)
import System.Process (readProcessWithExitCode)

-- | Does basic project setup for a project, including making sure
-- some directories exist, and pushing a new release directory with the
-- SHA1 or branch specified in the configuration.
pushRelease :: RC ()
pushRelease = setupDirs >> ensureRepositoryPushed >> updateCacheRepo >>
              cleanReleases >> cloneToRelease >> setReleaseRevision

-- | Switches the current symlink to point to the release specified in
-- the configuration. Maybe used in either deploy or rollback cases.
activateRelease :: RC (Maybe String)
activateRelease = removeCurrentSymlink >> symlinkCurrent

-- | Returns an initial state for the deploy.
initialState :: Config -> HapistranoState
initialState cfg = HapistranoState { config    = cfg
                                   , timestamp = Nothing
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

-- | Default method to run on deploy failure. Emits a failure message
-- and exits with a status code of 1.
defaultErrorHandler :: (Int, Maybe String) -> IO ()
defaultErrorHandler _ =
  hPutStrLn stderr "Deploy failed." >> exitWith (ExitFailure 1)

-- | Default method to run on deploy success.
defaultSuccessHandler :: a -> IO ()
defaultSuccessHandler _ = putStrLn "Deploy completed successfully."


-- | Creates necessary directories for the hapistrano project. Should
-- only need to run the first time the project is deployed on a given
-- system.
setupDirs :: RC ()
setupDirs = do
  conf <- gets config

  mapM_ (runCommand (host conf))
    ["mkdir -p " ++ releasesPath conf, "mkdir -p " ++ cacheRepoPath conf]

directoryExists :: Maybe String -> FilePath -> IO Bool
directoryExists hst path = do
  let (command, args) = case hst of
        Just h  -> ("ssh", [h, "ls", path])
        Nothing -> ("ls", [path])

  (code, _, _) <- readProcessWithExitCode command args ""

  return $ case code of
    ExitSuccess   -> True
    ExitFailure _ -> False

-- | Runs the given command either locally or on the local machine.
runCommand :: Maybe String -- ^ The host on which to run the command
           -> String -- ^ The command to run, either on the local or remote host
           -> RC (Maybe String)

runCommand Nothing command = do
  liftIO $ putStrLn $ "Going to execute " ++ command ++ " locally."

  let (cmd, args) = (head (words command), tail (words command))

  (code, stdout, err) <- liftIO $ readProcessWithExitCode cmd args ""

  case code of
    ExitSuccess -> do
      liftIO $
        putStrLn $ "Command '" ++ command ++ "' was successful on local host."

      unless (null stdout) (liftIO $ putStrLn $ "Output:\n" ++ stdout)

      lift $ right $ maybeString stdout

    ExitFailure int -> do
      let maybeError = maybeString err
      liftIO $ printCommandError "localhost" command (int, maybeError)
      lift $ left (int, maybeError)

runCommand (Just server) command = do
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
currentTimestamp :: ReleaseFormat -> IO String
currentTimestamp format = do
  curTime <- getCurrentTime
  return $ formatTime defaultTimeLocale fstring curTime

  where fstring = case format of
          Short -> "%Y%m%d%H%M%S"
          Long  -> "%Y%m%d%H%M%S%q"

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

-- | Returns the FilePath pointed to by the current symlink.
readCurrentLink :: Maybe String -> FilePath -> IO FilePath
readCurrentLink hst path = do
  let (command, args) = case hst of
        Just h  -> ("ssh", [h, "readlink", path])
        Nothing -> ("readlink", [path])

  (code, stdout, _) <- readProcessWithExitCode command args ""

  case (code, stdout) of
    (ExitSuccess, out) -> return $ trim out
    (ExitFailure _, _) -> error "Unable to read current symlink"

  where trim = reverse . dropWhile (=='\n') . reverse

-- | Ensure that the initial bare repo exists in the repo directory. Idempotent.
ensureRepositoryPushed :: RC (Maybe String)
ensureRepositoryPushed = do
  conf <- gets config
  res  <- liftIO $ directoryExists (host conf) $ joinPath [cacheRepoPath conf, "refs"]

  if res
    then lift $ right $ Just "Repo already existed"
    else createCacheRepo

-- | Returns a Just String or Nothing based on whether the input is null or
-- has contents.
maybeString :: String -> Maybe String
maybeString possibleString =
  if null possibleString then Nothing else Just possibleString

-- | Returns the full path of the folder containing all of the release builds.
releasesPath :: Config -> FilePath
releasesPath conf = joinPath [deployPath conf, "releases"]

-- | Figures out the most recent release if possible, and sets the
-- StateT monad with the correct timestamp. This function is used
-- before rollbacks.
detectPrevious :: [String] -> RC (Maybe String)
detectPrevious rs = do
  let mostRecentRls = biggest rs

  case mostRecentRls of
    Nothing -> lift $ left (1, Just "No previous releases detected!")
    Just rls -> do
      conf <- get
      _ <- put $ conf { timestamp = mostRecentRls }
      lift $ right $ Just rls

-- | Activates the previous detected release.
rollback :: RC (Maybe String)
rollback = previousReleases >>= detectPrevious >> activateRelease

-- | Clones the repository to the next releasePath timestamp. Makes a new
-- timestamp if one doesn't yet exist in the HapistranoState.
cloneToRelease :: RC (Maybe String)
cloneToRelease = do
  hs <- get
  conf <- gets config

  rls <- case timestamp hs of
           Nothing -> do
             ts <- liftIO $ currentTimestamp (releaseFormat conf)
             _ <- put $ hs { timestamp = Just ts }
             return ts

           Just r -> return r

  runCommand (host conf) $ "git clone " ++ cacheRepoPath (config hs) ++
    " " ++ joinPath [ releasesPath (config hs), rls ]


-- | Returns the full path to the git repo used for cache purposes on the
-- target host filesystem.
cacheRepoPath :: Config -> FilePath
cacheRepoPath conf = joinPath [deployPath conf, "repo"]

-- | Returns the full path to the current symlink.
currentPath :: FilePath -> FilePath
currentPath depPath = joinPath [depPath, "current"]

-- | Take the release timestamp from the end of a filepath.
pathToRelease :: FilePath -> Release
pathToRelease = last . splitPath

-- | Returns a list of Strings representing the currently deployed releases.
releases :: RC [Release]
releases = do
  conf <- gets config
  res  <- runCommand (host conf) $ "find " ++ releasesPath conf ++
          " -type d -maxdepth 1"

  case res of
    Nothing -> lift $ right []
    Just s ->
      lift $ right $
      filter (isReleaseString (releaseFormat conf)) . map pathToRelease $
      lines s

previousReleases :: RC [Release]
previousReleases = do
  rls <- releases
  conf <- gets config

  currentRelease <-
    liftIO $ readCurrentLink (host conf) (currentPath (deployPath conf))

  let currentRel = (head . lines . pathToRelease) currentRelease
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
  conf        <- gets config
  allReleases <- releases

  let deletable = oldReleases conf allReleases

  if null deletable
    then
      echoMessage "There are no old releases to prune."

    else
      runCommand (host conf) $
      "rm -rf -- " ++ unwords deletable

-- | Returns a Bool indicating if the given String is in the proper release
-- format.
isReleaseString :: ReleaseFormat -> String -> Bool
isReleaseString format s = all isNumber s && length s == releaseLength
  where releaseLength = case format of
          Short -> 14
          Long  -> 26

-- | Creates the git repository that is used on the target host for
-- cache purposes.
createCacheRepo :: RC (Maybe String)
createCacheRepo = do
  conf <- gets config

  runCommand (host conf) $ "git clone --bare " ++ repository conf ++ " " ++
    cacheRepoPath conf

-- | Returns the full path of the symlink pointing to the current
-- release.
currentSymlinkPath :: Config -> FilePath
currentSymlinkPath conf = joinPath [deployPath conf, "current"]

currentTempSymlinkPath :: Config -> FilePath
currentTempSymlinkPath conf = joinPath [deployPath conf, "current_tmp"]

-- | Removes the current symlink in preparation for a new release being
-- activated.
removeCurrentSymlink :: RC (Maybe String)
removeCurrentSymlink = do
  conf <- gets config

  runCommand (host conf) $ "rm -rf " ++ currentSymlinkPath conf

-- | Determines whether the target host OS is Linux
targetIsLinux :: RC Bool
targetIsLinux = do
  conf <- gets config
  res <- runCommand (host conf) "uname"

  case res of
    Just output -> lift $ right $ "Linux" `isInfixOf` output
    _ -> lift $ left (1, Just "Unable to determine remote host type")

-- | Runs a command to restart a server if a command is provided.
restartServerCommand :: RC (Maybe String)
restartServerCommand = do
  conf <- gets config

  case restartCommand conf of
    Nothing -> return $ Just "No command given for restart action."
    Just cmd -> runCommand (host conf) cmd

-- | Runs a build script if one is provided.
runBuild :: RC (Maybe String)
runBuild = do
  conf <- gets config

  case buildScript conf of
    Nothing -> do
      liftIO $ putStrLn "No build script specified, skipping build step."
      return Nothing

    Just scr -> do
      fl <- liftIO $ readFile scr
      let commands = lines fl
      buildRelease commands

-- | Returns the best 'mv' command for a symlink given the target platform.
mvCommand :: Bool   -- ^ Whether the target host is Linux
          -> String -- ^ The best mv command for a symlink on the platform
mvCommand True  = "mv -Tf"
mvCommand False = "mv -f"

-- | Creates a symlink to the current release.
lnCommand ::
  String    -- ^ The path of the new release
  -> String -- ^ The temporary symlink target for the release
  -> String -- ^ A command to create the temporary symlink
lnCommand rlsPath symlinkPath = unwords ["ln -s", rlsPath, symlinkPath]

-- | Creates a symlink to the directory indicated by the release timestamp.
-- hapistrano does this by creating a temporary symlink and doing an atomic
-- mv (1) operation to activate the new release.
symlinkCurrent :: RC (Maybe String)
symlinkCurrent = do
  conf             <- gets config
  releaseTimestamp <- gets timestamp

  case releaseTimestamp of
    Nothing  -> lift $ left (1, Just "No releases to symlink!")
    Just rls -> do
      isLnx <- targetIsLinux

      let tmpLnCmd =
            lnCommand (releasePath conf rls) (currentTempSymlinkPath conf)

      _ <- runCommand (host conf) $ tmpLnCmd

      runCommand (host conf) $ unwords [ mvCommand isLnx
                                       , currentTempSymlinkPath conf
                                       , currentSymlinkPath conf ]


-- | Updates the git repo used as a cache in the target host filesystem.
updateCacheRepo :: RC ()
updateCacheRepo = do
  conf <- gets config

  void $ runCommand (host conf) $ intercalate " && "
    [ "cd " ++ cacheRepoPath conf
    , "git fetch origin +refs/heads/*:refs/heads/*" ]

-- | Sets the release to the correct revision by resetting the
-- head of the git repo.
setReleaseRevision :: RC ()
setReleaseRevision = do
  conf             <- gets config
  releaseTimestamp <- gets timestamp

  case releaseTimestamp of
    Nothing -> lift $ left (1, Just "No releases to symlink!")
    Just rls -> do
      void $ runCommand (host conf) $ intercalate " && "
        [ "cd " ++ releasePath conf rls
        , "git fetch --all"
        , "git reset --hard " ++ revision conf
        ]

-- | Returns a command that builds this application. Sets the context
-- of the build by switching to the release directory before running
-- the script.
buildRelease :: [String] -- ^ Commands to be run. List intercalated
                         -- with "&&" so that failure aborts the
                         -- sequence.
             -> RC (Maybe String)
buildRelease commands = do
  conf             <- gets config
  releaseTimestamp <- gets timestamp

  case releaseTimestamp of
    Nothing -> lift $ left (1, Just "No releases to symlink!")
    Just rls -> do
      let cdCmd = "cd " ++ releasePath conf rls
      runCommand (host conf) $ intercalate " && " $ cdCmd : commands

-- | A safe version of the `maximum` function in Data.List.
biggest :: Ord a => [a] -> Maybe a
biggest rls =
  case sortBy (flip compare) rls of
    []  -> Nothing
    r:_ -> Just r
