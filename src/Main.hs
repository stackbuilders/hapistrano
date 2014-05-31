{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

------------------------------------------------------------------------------
import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Process
import System.Exit (ExitCode(..))
import Control.Lens
import Control.Monad (void, unless)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either (EitherT(..), left, right, runEitherT, eitherT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.IO (hPutStrLn, stderr)
import Data.List (intercalate, sortBy)
import Data.Char (isNumber)

------------------------------------------------------------------------------
-- ^ Config stuff that will be replaced by config file reading
data Config = Config { _deployPath :: String
                     , _host       :: String
                     , _repository :: String -- ^ The remote git repo
                     , _revision   :: String -- ^ A SHA1 or branch to release
                     } deriving (Show)

makeLenses ''Config

------------------------------------------------------------------------------
data HapistranoState = HapistranoState { _config    :: Config
                                       , _timestamp :: String
                                       }
makeLenses ''HapistranoState

------------------------------------------------------------------------------
type RC a = StateT HapistranoState (EitherT (Int, Maybe String) IO) a

------------------------------------------------------------------------------
runRC :: ((Int, Maybe String) -> IO a) -- Error handler
      -> (a -> IO a)                   -- Success handler
      -> HapistranoState               -- Initial state
      -> RC a
      -> IO a
runRC errorHandler successHandler initialState remoteCommand  =
    eitherT errorHandler
            successHandler
            (evalStateT remoteCommand initialState)

------------------------------------------------------------------------------
setupDirs :: RC (Maybe String)
setupDirs = do
  hostName <- use $ config . host
  pathName <- use $ config . deployPath
  remoteT $ "mkdir -p " ++ pathName ++ "/releases"

------------------------------------------------------------------------------
remoteT :: String -- ^ The command to run remotely
        -> RC (Maybe String) -- ^ Left (non-zero code, Maybe STDERR) or Right (Maybe STDOUT)
remoteT command = do
  server <- use $ config . host
  liftIO $ putStrLn $ "Going to execute " ++ command ++ " on host " ++ server ++ "."

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

------------------------------------------------------------------------------
currentTimestamp :: IO String
currentTimestamp = do
  curTime <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y%m%d%H%M%S" curTime

------------------------------------------------------------------------------
echoMessage :: String -> RC (Maybe String)
echoMessage msg = do
  liftIO $ putStrLn msg
  lift $ right Nothing
------------------------------------------------------------------------------
printCommandError :: String -> String -> (Int, Maybe String) -> IO ()
printCommandError server cmd (errCode, Nothing) =
  hPutStrLn stderr $ "Command " ++ " '" ++ cmd ++ "' failed on host '" ++
  server ++ "' with error code " ++ show errCode ++ " and no STDERR output."
printCommandError server cmd (errCode, Just errMsg) =
  hPutStrLn stderr $ "Command " ++ " '" ++ cmd ++ "' failed on host '" ++
  server ++ "' with error code " ++ show errCode ++ " and message '" ++
  errMsg ++ "'."

------------------------------------------------------------------------------
directoryExists :: String -> RC (Maybe String)
directoryExists path =
  remoteT $ "ls " ++  path

------------------------------------------------------------------------------
-- ^ Ensure that the initial bare repo exists in the repo directory. Idempotent.
ensureRepositoryPushed :: RC (Maybe String)
ensureRepositoryPushed = do
  state <- get
  config <- use config
  let e = runStateT (directoryExists (cacheRepoPath config)) state
  res <- liftIO $ runEitherT e

  case res of
    Left _ -> createCacheRepo
    Right _ -> lift $ right $ Just "Repo already existed"

------------------------------------------------------------------------------
-- | Returns a Just String or Nothing based on whether the input is null or
-- has contents.
maybeString :: String -> Maybe String
maybeString possibleString =
  if null possibleString then Nothing else Just possibleString

------------------------------------------------------------------------------
releasesPath :: Config -> String
releasesPath config = (config ^. deployPath) ++ "/releases"

------------------------------------------------------------------------------
-- | The path indicating the current release folder.
releasePath :: Config -> IO String
releasePath config = do
  ts <- currentTimestamp
  return $ releasesPath config ++ "/" ++ ts

------------------------------------------------------------------------------
-- | Clones the repository to the next releasePath timestamp.
cloneToRelease :: RC (Maybe String)
cloneToRelease = do
  config <- use config
  releaseTimestamp <- use timestamp
  remoteT $
    "git clone " ++ cacheRepoPath config ++ " " ++ releasesPath config ++ "/" ++
    releaseTimestamp

------------------------------------------------------------------------------
cacheRepoPath :: Config -> String
cacheRepoPath config = config ^. deployPath ++ "/repo"

------------------------------------------------------------------------------
releases :: RC [String]
releases = do
  state  <- get
  config <- use config
  res    <- liftIO $ runEitherT
            (evalStateT (remoteT ("find " ++ releasesPath config ++
                                  " -type d -maxdepth 1")) state)

  case res of
    Left r -> lift $ left r
    Right Nothing -> lift $ right []
    Right (Just s) ->
      lift $ right $ filter isReleaseString . map (reverse . take 14 . reverse) $
      lines s

------------------------------------------------------------------------------
-- | Given a list of release strings, takes the last five in the sequence.
-- Assumes a list of folders that has been determined to be a proper release
-- path.
oldReleases :: Config -> [String] -> [String]
oldReleases config rs = map mergePath toDelete
  where sorted             = sortBy (flip compare) rs
        toDelete           = drop 5 sorted
        mergePath fileName = releasesPath config ++ "/" ++ fileName

------------------------------------------------------------------------------
cleanReleases :: RC (Maybe String)
cleanReleases = do
  state <- get
  config <- use config
  allReleases <- liftIO $ runEitherT $ evalStateT releases state

  case allReleases of
    Left err -> lift $ left err
    Right [] -> echoMessage "There are no old releases to prune."
    Right xs -> do
      let deletable = oldReleases config xs

      remoteT $ "rm -rf " ++ foldr (\a b -> a ++ " " ++ b) ""
        deletable

------------------------------------------------------------------------------
isReleaseString :: String -> Bool
isReleaseString s = all isNumber s && length s == 14

------------------------------------------------------------------------------
createCacheRepo :: RC (Maybe String)
createCacheRepo = do
  conf <- use config
  remoteT $ "git clone --bare " ++ conf ^. repository ++ " " ++ cacheRepoPath
    conf ++ " && git reset --hard " ++ conf ^. revision

------------------------------------------------------------------------------
currentSymlinkPath :: Config -> String
currentSymlinkPath config = config ^. deployPath ++ "/current"

------------------------------------------------------------------------------
removeCurrentSymlink :: RC (Maybe String)
removeCurrentSymlink = do
  config <- use config
  remoteT $ "rm -rf " ++ currentSymlinkPath config

------------------------------------------------------------------------------
newestReleasePath :: Config -> [String] -> Maybe String
newestReleasePath _ [] = Nothing
newestReleasePath config rls = Just $ releasesPath config ++ "/" ++ maximum rls

------------------------------------------------------------------------------
symlinkCurrent :: RC (Maybe String)
symlinkCurrent = do
  state <- get
  config <- use config
  allReleases <- liftIO . runEitherT $ evalStateT releases state

  case allReleases of
    Left err -> lift $ left err
    Right [] -> lift $ left (1, Just "No releases to symlink!")
    Right rls -> do
      let latest = releasesPath config ++ "/" ++ maximum rls
      remoteT $ "ln -s " ++  latest ++ " " ++ currentSymlinkPath config

------------------------------------------------------------------------------
testConfig :: Config
testConfig = Config { _deployPath = "/tmp/project"
                    , _host       = "localhost"
                    , _repository = "/tmp/testrepo"
                    , _revision    = "master"
                    }

------------------------------------------------------------------------------
updateCacheRepo :: RC (Maybe String)
updateCacheRepo = do
  config <- use config
  remoteT $ cmd config
  where cmd config = "cd " ++ (cacheRepoPath config) ++ " && " ++
                     "git fetch origin +refs/heads/*:refs/heads/*"

------------------------------------------------------------------------------
buildRelease :: RC (Maybe String)
buildRelease  = do
  config <- use config
  releaseTimestamp <- use timestamp
  remoteT $ cmd releaseTimestamp config
  where cmd releaseTimestamp config = intercalate " && "
              [ "cd " ++ releasesPath config ++ "/" ++ releaseTimestamp
              , "export PATH=~/.cabal/bin:/usr/local/bin:$PATH"
              , "git fetch --all"
              , "git reset --hard origin/master"
              , "rm -rf .cabal-sandbox"
              , "cabal sandbox init"
              , "cabal clean"
              , "cabal update"
              , "cabal install --only-dependencies -j"
              , "cabal build -j" ]

------------------------------------------------------------------------------
initialState :: IO HapistranoState
initialState = do
  timestamp <- currentTimestamp
  return $ HapistranoState { _config    = testConfig
                           , _timestamp = timestamp
                           }

------------------------------------------------------------------------------
main :: IO ()
main = do
  initState <- initialState
  void $ runRC errorHandler successHandler  initState $ do
           setupDirs
           ensureRepositoryPushed
           updateCacheRepo
           cleanReleases
           cloneToRelease
           buildRelease
           removeCurrentSymlink
           symlinkCurrent
  where
    errorHandler   = undefined
    successHandler = undefined
