{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A module for easily creating reliable deploy processes for Haskell
-- applications.
module Hapistrano
       (
         Config(..)
       , initialState
       , runRC
       , pushRelease
       , activateRelease
       , defaultBuildRelease
       , defaultSuccessHandler
       , defaultErrorHandler
       , currentTimestamp
       ) where

import Control.Lens (makeLenses, use, (^.))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.State (StateT, evalStateT, runStateT, get)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either ( EitherT(..)
                                  , left
                                  , right
                                  , runEitherT
                                  , eitherT )
import Data.Char (isNumber)
import Data.List (intercalate, sortBy)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Exit (ExitCode(..))
import System.FilePath.Posix (joinPath)
import System.IO (hPutStrLn, stderr)
import System.Locale (defaultTimeLocale)
import System.Process (readProcessWithExitCode)

-- ^ Config stuff that will be replaced by config file reading
data Config = Config { _deployPath :: String
                     , _host       :: String
                     , _repository :: String -- ^ The remote git repo
                     , _revision   :: String -- ^ A SHA1 or branch to release
                     } deriving (Show)

makeLenses ''Config


data HapistranoState = HapistranoState { _config    :: Config
                                       , _timestamp :: String
                                       }
makeLenses ''HapistranoState


type RC a = StateT HapistranoState (EitherT (Int, Maybe String) IO) a

initialState :: Config -> IO HapistranoState
initialState cfg = do
  ts <- currentTimestamp
  return HapistranoState { _config    = cfg
                         , _timestamp = ts
                         }
-- | Given a pair of actions, one to perform in case of failure, and
-- one to perform in case of success, run an EitherT and get back a
-- monadic result.
runRC :: ((Int, Maybe String) -> IO a) -- ^ Error handler
      -> (a -> IO a)                   -- ^ Success handler
      -> HapistranoState               -- ^ Initial state
      -> RC a
      -> IO a
runRC errorHandler successHandler initState remoteCommand  =
    eitherT errorHandler
            successHandler
            (evalStateT remoteCommand initState)

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

-- | Ensure that the initial bare repo exists in the repo directory. Idempotent.
ensureRepositoryPushed :: RC (Maybe String)
ensureRepositoryPushed = do
  st <- get
  conf <- use config
  let e = runStateT (directoryExists (cacheRepoPath conf)) st
  res <- liftIO $ runEitherT e

  case res of
    Left _ -> createCacheRepo
    Right _ -> lift $ right $ Just "Repo already existed"

-- | Returns a Just String or Nothing based on whether the input is null or
-- has contents.
maybeString :: String -> Maybe String
maybeString possibleString =
  if null possibleString then Nothing else Just possibleString

-- | Returns the full path of the folder containing all of the release builds.
releasesPath :: Config -> FilePath
releasesPath conf = joinPath [(conf ^. deployPath), "releases"]

-- | Clones the repository to the next releasePath timestamp.
cloneToRelease :: RC (Maybe String)
cloneToRelease = do
  conf <- use config
  releaseTimestamp <- use timestamp
  remoteCommand $
    "git clone " ++ cacheRepoPath conf ++ " " ++ joinPath [releasesPath conf, releaseTimestamp]

-- | Returns the full path to the git repo used for cache purposes on the
-- target host filesystem.
cacheRepoPath :: Config -> FilePath
cacheRepoPath conf = joinPath [conf ^. deployPath, "repo"]

-- | Returns a list of Strings representing the currently deployed releases.
releases :: RC [String]
releases = do
  st  <- get
  conf <- use config
  res    <- liftIO $ runEitherT
            (evalStateT (remoteCommand ("find " ++ releasesPath conf ++
                                  " -type d -maxdepth 1")) st)

  case res of
    Left r -> lift $ left r
    Right Nothing -> lift $ right []
    Right (Just s) ->
      lift $ right $ filter isReleaseString . map (reverse . take 14 . reverse)
      $ lines s

-- | Given a list of release strings, takes the last five in the sequence.
-- Assumes a list of folders that has been determined to be a proper release
-- path.
oldReleases :: Config -> [String] -> [FilePath]
oldReleases conf rs = map mergePath toDelete
  where sorted             = sortBy (flip compare) rs
        toDelete           = drop 5 sorted
        mergePath fileName = joinPath [releasesPath conf, fileName]

-- | Removes releases older than the last five to avoid filling up the target
-- host filesystem.
cleanReleases :: RC (Maybe String)
cleanReleases = do
  st <- get
  conf <- use config
  allReleases <- liftIO $ runEitherT $ evalStateT releases st

  case allReleases of
    Left err -> lift $ left err
    Right [] -> echoMessage "There are no old releases to prune."
    Right xs -> do
      let deletable = oldReleases conf xs

      remoteCommand $ "rm -rf --" ++ foldr (\a b -> a ++ " " ++ b) ""
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

-- | Removes the current symlink in preparation for a new release being
-- activated.
removeCurrentSymlink :: RC (Maybe String)
removeCurrentSymlink = do
  conf <- use config
  remoteCommand $ "rm -rf " ++ currentSymlinkPath conf

-- | Creates a symlink to the directory indicated by the release timestamp.
symlinkCurrent :: RC (Maybe String)
symlinkCurrent = do
  st <- get
  conf <- use config
  allReleases <- liftIO . runEitherT $ evalStateT releases st

  case allReleases of
    Left err -> lift $ left err
    Right [] -> lift $ left (1, Just "No releases to symlink!")
    Right rls -> do
      let latest = joinPath [releasesPath conf, maximum rls]
      remoteCommand $ "ln -s " ++  latest ++ " " ++ currentSymlinkPath conf

-- | Updates the git repo used as a cache in the target host filesystem.
updateCacheRepo :: RC (Maybe String)
updateCacheRepo = do
  conf <- use config
  remoteCommand $ intercalate " && "
    [ "cd " ++ cacheRepoPath conf
    , "git fetch origin +refs/heads/*:refs/heads/*" ]

-- | Does a default, conservative build of the project directory by
-- completely re-installing all dependencies in a sandbox. This exact
-- process is likely to be different depending on your application and
-- environment, so feel free to create your own version of this
-- function.
defaultBuildRelease :: RC (Maybe String)
defaultBuildRelease  = do
  conf <- use config
  releaseTimestamp <- use timestamp
  remoteCommand $ intercalate " && "
              [ "cd " ++ joinPath [releasesPath conf, releaseTimestamp]
              , "export PATH=~/.cabal/bin:/usr/local/bin:$PATH"
              , "git fetch --all"
              , "git reset --hard " ++ conf ^. revision
              , "rm -rf .cabal-sandbox"
              , "cabal sandbox init"
              , "cabal clean"
              , "cabal update"
              , "cabal install --only-dependencies -j"
              , "cabal build -j" ]

-- | Does basic project setup for a project, including making sure
-- some directories exist, and pushing a new release directory with the
-- SHA1 or branch specified in the configuration.
pushRelease :: RC (Maybe String)
pushRelease = do
  setupDirs
  ensureRepositoryPushed
  updateCacheRepo
  cleanReleases
  cloneToRelease

-- | Switches the current symlink to point to the release specified in
-- the configuration.
activateRelease :: RC (Maybe String)
activateRelease = removeCurrentSymlink >> symlinkCurrent
