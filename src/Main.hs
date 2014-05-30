module Main where

import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Process
import System.Exit (ExitCode(..))
import Control.Monad.Trans.Either (EitherT(..), left, right, runEitherT)
import Control.Monad.IO.Class (MonadIO(liftIO))

import System.IO (hPutStrLn, stderr)

import Data.List

import Data.Char (isNumber)

currentTimestamp :: IO String
currentTimestamp = do
  curTime <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y%m%d%H%M%S" curTime

type RemoteCommand = EitherT (Int, Maybe String) IO (Maybe String)

-- | Used to compose a "command" that is really just output in the chain.
echoMessage :: String -> RemoteCommand
echoMessage msg = do
  liftIO $ putStrLn msg
  right Nothing

printCommandError :: String -> String -> (Int, Maybe String) -> IO ()
printCommandError server cmd (errCode, Nothing) =
  hPutStrLn stderr $ "Command " ++ " '" ++ cmd ++ "' failed on host '" ++
  server ++ "' with error code " ++ show errCode ++ " and no STDERR output."
printCommandError server cmd (errCode, Just errMsg) =
  hPutStrLn stderr $ "Command " ++ " '" ++ cmd ++ "' failed on host '" ++
  server ++ "' with error code " ++ show errCode ++ " and message '" ++
  errMsg ++ "'."

remoteT ::  -- ^ The host to run commands on
            String

            -- ^ The command to run remotely
            -> String

            -- ^ Left (non-zero code, Maybe STDERR) or Right (Maybe STDOUT)
            -> RemoteCommand

remoteT server command = do
  liftIO $ putStrLn $ "Going to execute " ++ command ++ " on host " ++ server ++
    "."

  (code, stdout, err) <-
    liftIO $ readProcessWithExitCode "ssh" (server : words command) ""

  case code of
    ExitSuccess -> do
      liftIO $ putStrLn $ "Command '" ++ command ++
        "' was successful on host '" ++ server ++ "'."

      right $ maybeString stdout
    ExitFailure int -> do
      let maybeError = maybeString err
      liftIO $ printCommandError server command (int, maybeError)
      left $ (int, maybeError)

directoryExists :: String -> String -> RemoteCommand
directoryExists server path = remoteT server $ "ls " ++ path

-- ^ Ensure that the initial bare repo exists in the repo directory. Idempotent.
ensureRepositoryPushed :: Config -> RemoteCommand
ensureRepositoryPushed config = do
  res <- liftIO $ runEitherT $ directoryExists (host config) (cacheRepoPath config)

  case res of
    Left _ -> createCacheRepo config
    Right _ -> right $ Just "Repo already existed"


-- ^ Config stuff that will be replaced by config file reading
data Config = Config { deployPath :: String
                     , deploySha1 :: String
                     , host       :: String
                     , repository :: String -- ^ The remote git repo
                     } deriving (Show)


-- | Returns a Just String or Nothing based on whether the input is null or
-- has contents.
maybeString :: String -> Maybe String
maybeString possibleString =
  if null possibleString then Nothing else Just possibleString

releasesPath :: Config -> String
releasesPath config = deployPath config ++ "/releases"

-- | The path indicating the current release folder.
releasePath :: Config -> IO String
releasePath config = do
  ts <- currentTimestamp
  return $ releasesPath config ++ "/" ++ ts

-- | Clones the repository to the next releasePath timestamp.
cloneToRelease :: Config -> String -> RemoteCommand
cloneToRelease config releaseTimestamp = do
  remoteT (host config) $
    "git clone " ++ cacheRepoPath config ++ " " ++ releasesPath config ++ "/" ++
    releaseTimestamp

cacheRepoPath :: Config -> String
cacheRepoPath config = deployPath config ++ "/repo"

releases :: Config -> EitherT (Int, Maybe String) IO [String]
releases config = do
  res <- liftIO $ runEitherT $ remoteT (host config) $
         "find " ++ releasesPath config ++ " -type d -maxdepth 1"

  case res of
    Left r -> left r
    Right Nothing -> right []
    Right (Just s) ->
      right $ filter isReleaseString . map (reverse . take 14 . reverse) $
      lines s

-- | Given a list of release strings, takes the last five in the sequence.
-- Assumes a list of folders that has been determined to be a proper release
-- path.
oldReleases :: Config -> [String] -> [String]
oldReleases config rs =
  withDir
  where sorted = (reverse . sort) rs
        toDelete = drop 5 sorted
        withDir = map (\fileName -> (releasesPath config) ++ "/" ++ fileName)
                  toDelete

cleanReleases :: Config -> RemoteCommand
cleanReleases config = do
  allReleases <- liftIO $ runEitherT $ releases config

  case allReleases of
    Left err -> left err
    Right [] -> echoMessage "There are no old releases to prune."
    Right xs -> do
      let deletable = oldReleases config xs

      remoteT (host config) $ "rm -rf " ++ foldr (\a b -> a ++ " " ++ b) ""
        deletable

isReleaseString :: String -> Bool
isReleaseString s = all isNumber s && (length s) == 14

createCacheRepo :: Config -> RemoteCommand
createCacheRepo config =
  remoteT (host config) cmd
  where cmd = "git clone --bare " ++ (repository config) ++ " " ++
              cacheRepoPath config


setupDirs :: Config -> RemoteCommand
setupDirs config =
  remoteT (host config) $ "mkdir -p " ++ (deployPath config) ++ "/releases"

currentSymlinkPath :: Config -> String
currentSymlinkPath config = deployPath config ++ "/current"

removeCurrentSymlink :: Config -> RemoteCommand
removeCurrentSymlink config = remoteT (host config) $
                              "rm -rf " ++ currentSymlinkPath config


newestReleasePath :: Config -> [String] -> Maybe String
newestReleasePath _ [] = Nothing
newestReleasePath config rls = Just $ releasesPath config ++ "/" ++
                                    (head . reverse . sort) rls

symlinkCurrent :: Config -> RemoteCommand
symlinkCurrent config = do
  allReleases <- liftIO $ runEitherT $ releases config

  case allReleases of
    Left err -> left err
    Right [] -> left (1, Just "No releases to symlink!")
    Right rls -> do
      let latest = releasesPath config ++ "/" ++ (head . reverse . sort) rls
      remoteT (host config) $ "ln -s " ++  latest ++ " " ++
        (currentSymlinkPath config)

testConfig :: Config
testConfig = Config { deployPath = "/tmp/project"
                    , deploySha1 = "master"
                    , host       = "localhost"
                    , repository = "/tmp/testrepo"
                    }

updateCacheRepo :: Config -> RemoteCommand
updateCacheRepo config =
  remoteT (host config) cmd
  where cmd = "cd " ++ (cacheRepoPath config) ++ " && " ++
              "git fetch origin +refs/heads/*:refs/heads/*"

buildRelease :: Config -> String -> RemoteCommand
buildRelease config releaseTimestamp = remoteT (host config) cmd
  where cmd = intercalate " && "
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

main :: IO ()
main = do
  releaseTimestamp <- currentTimestamp
  runEitherT $
    setupDirs testConfig >>
    ensureRepositoryPushed testConfig >>
    updateCacheRepo testConfig >>
    cleanReleases testConfig >>
    cloneToRelease testConfig releaseTimestamp >>
    buildRelease testConfig releaseTimestamp >>
    removeCurrentSymlink testConfig >>
    symlinkCurrent testConfig

  return ()
