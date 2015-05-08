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
       , isReleaseString
       , pathToRelease
       , pushRelease
       , readCurrentLink
       , restartServerCommand
       , rollback
       , runRC
       , runBuild

       ) where

import Control.Monad.Reader (ReaderT(..), ask)


import System.Hapistrano.Types
  (Config(..), FailureResult, Hapistrano, Release, ReleaseFormat(..))

import Control.Monad (unless, void)
import System.Exit (ExitCode(..), exitWith)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Either ( left
                                  , right
                                  , eitherT )

import Data.Char (isNumber)
import Data.List (intercalate, sortBy, isInfixOf)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.FilePath.Posix (joinPath, splitPath)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | Does basic project setup for a project, including making sure
-- some directories exist, and pushing a new release directory with the
-- SHA1 or branch specified in the configuration.
pushRelease :: Hapistrano Release
pushRelease = setupDirs >> ensureRepositoryPushed >> updateCacheRepo >>
              cleanReleases >> cloneToRelease >>= setReleaseRevision

-- | Switches the current symlink to point to the release specified in
-- the configuration. Maybe used in either deploy or rollback cases.
activateRelease :: Release -> Hapistrano String
activateRelease rel = removeCurrentSymlink >> symlinkCurrent rel

-- | Runs the deploy, along with an optional success or failure function.
runRC :: ((Int, String) -> ReaderT Config IO a) -- ^ Error handler
      -> (a -> ReaderT Config IO a)             -- ^ Success handler
      -> Config                  -- ^ Hapistrano deployment configuration
      -> Hapistrano a            -- ^ The remote command to run
      -> IO a
runRC errorHandler successHandler config command =
    runReaderT (eitherT errorHandler successHandler command) config

-- | Default method to run on deploy failure. Emits a failure message
-- and exits with a status code of 1.
defaultErrorHandler :: FailureResult -> ReaderT Config IO ()
defaultErrorHandler res =
  liftIO $ hPutStrLn stderr
  ("Deploy failed with (status, message): " ++ show res)
  >> exitWith (ExitFailure 1)

-- | Default method to run on deploy success.
defaultSuccessHandler :: a -> ReaderT Config IO ()
defaultSuccessHandler _ =
  liftIO $ putStrLn "Deploy completed successfully."


-- | Creates necessary directories for the hapistrano project. Should
-- only need to run the first time the project is deployed on a given
-- system.
setupDirs :: Hapistrano ()
setupDirs = do
  conf <- ask

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
           -> Hapistrano String

runCommand Nothing command = execCommand command
runCommand (Just server) command =
  execCommand $ unwords ["ssh", server, command]


execCommand :: String -> Hapistrano String
execCommand cmd = do
  let wds         = words cmd
      (cmd', args) = (head wds, tail wds)

  liftIO $ putStrLn $ "Executing: " ++ cmd

  (code, stdout, err) <- liftIO $ readProcessWithExitCode cmd' args ""

  case code of
    ExitSuccess -> do
      unless (null stdout) (liftIO $ putStrLn $ "Output: " ++ stdout)

      right $ trim stdout

    ExitFailure int -> left (int, trim err)

-- | Returns a timestamp in the default format for build directories.
currentTimestamp :: ReleaseFormat -> IO String
currentTimestamp format = do
  curTime <- getCurrentTime
  return $ formatTime defaultTimeLocale fstring curTime

  where fstring = case format of
          Short -> "%Y%m%d%H%M%S"
          Long  -> "%Y%m%d%H%M%S%q"

-- | Returns the FilePath pointed to by the current symlink.
readCurrentLink :: Hapistrano FilePath -- ^ The target of the symlink in the Hapistrano monad
readCurrentLink = do
  conf <- ask
  runCommand (host conf) $ "readlink " ++ currentPath (deployPath conf)

-- ^ Trims any newlines from the given String
trim :: String -- ^ String to have trailing newlines stripped
     -> String -- ^ String with trailing newlines removed
trim = reverse . dropWhile (== '\n') . reverse

-- | Ensure that the initial bare repo exists in the repo directory. Idempotent.
ensureRepositoryPushed :: Hapistrano String
ensureRepositoryPushed = do
  conf <- ask
  res  <-
    liftIO $ directoryExists (host conf) $ joinPath [cacheRepoPath conf, "refs"]

  if res
    then right "Repo already existed"
    else createCacheRepo

-- | Returns the full path of the folder containing all of the release builds.
releasesPath :: Config -> FilePath
releasesPath conf = joinPath [deployPath conf, "releases"]

-- | Figures out the most recent release if possible.
detectPrevious :: [String] -- ^ The releases in `releases` path
               -> Hapistrano String -- ^ The previous release in the Hapistrano monad
detectPrevious rs =
  case biggest rs of
    Nothing -> left (1, "No previous releases detected!")
    Just rls -> right rls

-- | Activates the previous detected release.
rollback :: Hapistrano String -- ^ The current Release in the Hapistrano monad
rollback = previousReleases >>= detectPrevious >>= activateRelease

-- | Clones the repository to the next releasePath timestamp. Makes a new
-- timestamp if one doesn't yet exist in the HapistranoState. Returns the
-- timestamp of the release that we cloned to.
cloneToRelease :: Hapistrano Release -- ^ The newly-cloned Release, in the Hapistrano monad
cloneToRelease = do
  conf <- ask
  rls  <- liftIO $ currentTimestamp (releaseFormat conf)

  void $ runCommand (host conf) $ "git clone " ++ cacheRepoPath conf ++
    " " ++ joinPath [ releasesPath conf, rls ]

  return rls


-- | Returns the full path to the git repo used for cache purposes on the
-- target host filesystem.
cacheRepoPath :: Config -- ^ The Hapistrano configuration
              -> FilePath -- ^ The full path to the git cache repo used for speeding up deploys
cacheRepoPath conf = joinPath [deployPath conf, "repo"]

-- | Returns the full path to the current symlink.
currentPath :: FilePath -- ^ The full path of the deploy folder root
            -> FilePath -- ^ The full path to the `current` symlink
currentPath depPath = joinPath [depPath, "current"]

-- | Take the release timestamp from the end of a filepath.
pathToRelease :: FilePath -- ^ The entire FilePath to a Release directory
              -> Release -- ^ The Release number.
pathToRelease = last . splitPath

-- | Returns a list of Strings representing the currently deployed releases.
releases :: Hapistrano [Release] -- ^ A list of all found Releases on the target host
releases = do
  conf <- ask
  res  <- runCommand (host conf) $ "find " ++ releasesPath conf ++
          " -type d -maxdepth 1"

  right $
    filter (isReleaseString (releaseFormat conf)) . map pathToRelease $
    lines res

previousReleases :: Hapistrano [Release] -- ^ All non-current releases on the target host
previousReleases = do
  rls            <- releases
  currentRelease <- readCurrentLink

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
cleanReleases :: Hapistrano [String] -- ^ Deleted Release directories
cleanReleases = do
  conf        <- ask
  allReleases <- releases

  let deletable = oldReleases conf allReleases

  if null deletable
    then do
      liftIO $ putStrLn "There are no old releases to prune."
      return []

    else do
      _ <- runCommand (host conf) $ "rm -rf -- " ++ unwords deletable
      return deletable

-- | Returns a Bool indicating if the given String is in the proper release
-- format.
isReleaseString :: ReleaseFormat -- ^ Format of Release directories
                -> String -- ^ String to check against Release format
                -> Bool -- ^ Whether the given String adheres to the specified Release format
isReleaseString format s = all isNumber s && length s == releaseLength
  where releaseLength = case format of
          Short -> 14
          Long  -> 26

-- | Creates the git repository that is used on the target host for
-- cache purposes.
createCacheRepo :: Hapistrano String -- ^ Output of the git command used to create the bare cache repo
createCacheRepo = do
  conf <- ask

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
removeCurrentSymlink :: Hapistrano ()
removeCurrentSymlink = do
  conf <- ask

  void $ runCommand (host conf) $ "rm -rf " ++ currentSymlinkPath conf

-- | Determines whether the target host OS is Linux
targetIsLinux :: Hapistrano Bool
targetIsLinux = do
  conf <- ask
  res <- runCommand (host conf) "uname"

  right $ "Linux" `isInfixOf` res

-- | Runs a command to restart a server if a command is provided.
restartServerCommand :: Hapistrano String
restartServerCommand = do
  conf <- ask

  case restartCommand conf of
    Nothing -> return "No command given for restart action."
    Just cmd -> runCommand (host conf) cmd

-- | Runs a build script if one is provided.
runBuild :: Release -> Hapistrano Release
runBuild rel = do
  conf <- ask

  case buildScript conf of
    Nothing ->
      liftIO $ putStrLn "No build script specified, skipping build step."

    Just scr -> do
      fl <- liftIO $ readFile scr
      buildRelease rel $ lines fl

  right rel

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
symlinkCurrent :: Release -> Hapistrano String
symlinkCurrent rel = do
  conf <- ask

  isLnx <- targetIsLinux

  let tmpLnCmd =
        lnCommand (releasePath conf rel) (currentTempSymlinkPath conf)

  _ <- runCommand (host conf) tmpLnCmd

  runCommand (host conf) $ unwords [ mvCommand isLnx
                                   , currentTempSymlinkPath conf
                                   , currentSymlinkPath conf ]


-- | Updates the git repo used as a cache in the target host filesystem.
updateCacheRepo :: Hapistrano ()
updateCacheRepo = do
  conf <- ask

  void $ runCommand (host conf) $ intercalate " && "
    [ "cd " ++ cacheRepoPath conf
    , "git fetch origin +refs/heads/*:refs/heads/*" ]

-- | Sets the release to the correct revision by resetting the
-- head of the git repo.
setReleaseRevision :: Release -> Hapistrano Release
setReleaseRevision rel = do
  conf <- ask

  liftIO $ putStrLn "Setting revision in release path."

  void $ runCommand (host conf) $ intercalate " && "
    [ "cd " ++ releasePath conf rel
    , "git fetch --all"
    , "git reset --hard " ++ revision conf
    ]

  return rel

-- | Returns a command that builds this application. Sets the context
-- of the build by switching to the release directory before running
-- the script.
buildRelease :: Release  -- ^ The Release to build
             -> [String] -- ^ Commands to be run. List intercalated
                         -- with "&&" so that failure aborts the
                         -- sequence.
             -> Hapistrano ()
buildRelease rel commands = do
  conf <- ask
  let cdCmd = "cd " ++ releasePath conf rel
  void $ runCommand (host conf) $ intercalate " && " $ cdCmd : commands


-- | A safe version of the `maximum` function in Data.List.
biggest :: Ord a => [a] -> Maybe a
biggest rls =
  case sortBy (flip compare) rls of
    []  -> Nothing
    r:_ -> Just r
