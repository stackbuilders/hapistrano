-- |
-- Module      :  System.Hapistrano
-- Copyright   :  © 2015-Present Stack Builders
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for creating reliable deploy processes for Haskell applications.
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module System.Hapistrano
  ( runHapistrano
  , pushRelease
  , pushReleaseWithoutVc
  , activateRelease
  , linkToShared
  , createHapistranoDeployState
  , deploy
  , rollback
  , dropOldReleases
  , playScript
  , playScriptLocally
  , initConfig
    -- * Path helpers
  , releasePath
  , sharedPath
  , currentSymlinkPath
  , tempSymlinkPath
  , deployState )
where

import           Control.Exception          (try)
import           Control.Monad
import           Control.Monad.Catch        (catch, throwM)
import           Control.Monad.Except
import           Control.Monad.Reader       (local)
import           Data.List                  (dropWhileEnd, genericDrop, sortOn)
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Ord                   (Down (..))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Time
import qualified Data.Yaml                  as Yaml
import           Numeric.Natural
import           Path
import           Path.IO
import qualified System.Directory           as Directory
import           System.Exit                (exitFailure)
import qualified System.FilePath            as FilePath
import           System.Hapistrano.Commands
import qualified System.Hapistrano.Config   as HC
import           System.Hapistrano.Config   (BuildCommand (..), CopyThing (..),
                                             ExecutionMode (..),
                                             deployStateFilename)
import           System.Hapistrano.Core
import           System.Hapistrano.Types
import           System.IO                  (stderr)
import           Text.Read                  (readMaybe)

----------------------------------------------------------------------------

-- | Run the 'Hapistrano' monad. The monad hosts 'exec' actions.
runHapistrano ::
     MonadIO m
  => Maybe SshOptions -- ^ SSH options to use or 'Nothing' if we run locally
  -> Shell -- ^ Shell to run commands
  -> (OutputDest -> String -> IO ()) -- ^ How to print messages
  -> Hapistrano a -- ^ The computation to run
  -> m (Either Int a) -- ^ Status code in 'Left' on failure, result in
              -- 'Right' on success
runHapistrano sshOptions shell' printFnc m =
  liftIO $ do
    let config =
          Config
            { configSshOptions = sshOptions
            , configShellOptions = shell'
            , configPrint = printFnc
            }
    r <- try @HapistranoException $ unHapistrano m config
    case r of
      Left (HapistranoException (Failure n msg, _)) -> do
        forM_ msg (printFnc StderrDest)
        return (Left n)
      Right x -> return (Right x)

-- High-level functionality

-- | Perform basic setup for a project, making sure necessary directories
-- exist and pushing a new release directory with the SHA1 or branch
-- specified in the configuration. Return identifier of the pushed release.

pushRelease :: Task -> Hapistrano Release
pushRelease Task {..} = do
  setupDirs taskDeployPath
  pushReleaseForRepository taskSource
  where
    -- When the configuration is set for a local directory, it will only create
    -- the release directory without any version control operations.
    pushReleaseForRepository GitRepository {..} = do
      ensureCacheInPlace gitRepositoryURL taskDeployPath Nothing
      release <- newRelease taskReleaseFormat
      cloneToRelease taskDeployPath release
      setReleaseRevision taskDeployPath release gitRepositoryRevision
      return release
    pushReleaseForRepository LocalDirectory {} =
      newRelease taskReleaseFormat

-- | Same as 'pushRelease' but doesn't perform any version control
-- related operations.

pushReleaseWithoutVc :: Task -> Hapistrano Release
pushReleaseWithoutVc Task {..} = do
  setupDirs taskDeployPath
  newRelease taskReleaseFormat

-- | Switch the current symlink to point to the specified release. May be
-- used in deploy or rollback cases.

activateRelease
  :: TargetSystem
  -> Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ Release identifier to activate
  -> Hapistrano ()
activateRelease ts deployPath release = do
  rpath <- releasePath deployPath release Nothing
  let tpath = tempSymlinkPath deployPath
      cpath = currentSymlinkPath deployPath
  exec (Ln ts rpath tpath) (Just release) -- create a symlink for the new candidate
  exec (Mv ts tpath cpath) (Just release) -- atomically replace the symlink

-- | Creates the file @.hapistrano__state@ containing
-- @fail@ or @success@ depending on how the deployment ended.

createHapistranoDeployState
  :: Path Abs Dir -- ^ Deploy path
  -> Release -- ^ Release being deployed
  -> DeployState -- ^ Indicates how the deployment went
  -> Hapistrano ()
createHapistranoDeployState deployPath release state = do
  parseStatePath <- parseRelFile deployStateFilename
  actualReleasePath <- releasePath deployPath release Nothing
  let stateFilePath = actualReleasePath </> parseStatePath
  exec (Touch stateFilePath) (Just release) -- creates '.hapistrano_deploy_state'
  exec (BasicWrite stateFilePath $ show state) (Just release) -- writes the deploy state to '.hapistrano_deploy_state'

-- | Deploys a new release
deploy
  :: HC.Config -- ^ Deploy configuration
  -> ReleaseFormat -- ^ Long or Short format
  -> Natural -- ^ Number of releases to keep
  -> Bool -- ^ Whether we should keep one failed release or not
  -> ExecutionMode -- ^ Is running on lead target
  -> Hapistrano ()
deploy HC.Config{..} releaseFormat keepReleases keepOneFailed executionMode = do
  forM_ configRunLocally playScriptLocally
  release <- if configVcAction
              then pushRelease task
              else pushReleaseWithoutVc task
  rpath <- releasePath configDeployPath release configWorkingDir
  forM_ (toMaybePath configSource) $ \src ->
    scpDir src rpath (Just release)
  forM_ configCopyFiles $ \(CopyThing src dest) -> do
    srcPath  <- resolveFile' src
    destPath <- parseRelFile dest
    let dpath = rpath </> destPath
    (flip exec (Just release) . MkDir . parent) dpath
    scpFile srcPath dpath (Just release)
  forM_ configCopyDirs $ \(CopyThing src dest) -> do
    srcPath  <- resolveDir' src
    destPath <- parseRelDir dest
    let dpath = rpath </> destPath
    (flip exec (Just release) . MkDir . parent) dpath
    scpDir srcPath dpath (Just release)
  forM_ configLinkedFiles
    $ flip (linkToShared configTargetSystem rpath configDeployPath) (Just release)
  forM_ configLinkedDirs
    $ flip (linkToShared configTargetSystem rpath configDeployPath) (Just release)
  forM_ configBuildScript (playScript configDeployPath release configWorkingDir executionMode)
  activateRelease configTargetSystem configDeployPath release
  forM_ configRestartCommand (flip exec $ Just release)
  createHapistranoDeployState configDeployPath release Success
  dropOldReleases configDeployPath keepReleases keepOneFailed
  `catch` failStateAndThrow
    where
    failStateAndThrow e@(HapistranoException (_, maybeRelease)) = do
      case maybeRelease of
        (Just release) -> do
          createHapistranoDeployState configDeployPath release Fail
          dropOldReleases configDeployPath keepReleases keepOneFailed
          throwM e
        Nothing -> do
          throwM e
    task =
      Task
      { taskDeployPath    = configDeployPath
      , taskSource        = configSource
      , taskReleaseFormat = releaseFormat
      }

-- | Activates one of already deployed releases.

rollback
  :: TargetSystem
  -> Path Abs Dir      -- ^ Deploy path
  -> Natural           -- ^ How many releases back to go, 0 re-activates current
  -> Maybe GenericCommand -- ^ Restart command
  -> Hapistrano ()
rollback ts deployPath n mbRestartCommand = do
  releases <- releasesWithState Success deployPath
  case genericDrop n releases of
    [] -> failWith 1 (Just "Could not find the requested release to rollback to.") Nothing
    (x:_) -> do
      rpath <- releasePath deployPath x Nothing
      isRpathExist <- doesDirExist rpath
      if isRpathExist
      then activateRelease ts deployPath x
      else failWith 1 (Just $ "Cannot rollback to the release path '" <> show rpath <> "'.") (Just x)
  forM_ mbRestartCommand (`exec` Nothing)

-- | Remove older releases to avoid filling up the target host filesystem.

dropOldReleases
  :: Path Abs Dir      -- ^ Deploy path
  -> Natural           -- ^ How many releases to keep
  -> Bool              -- ^ Whether the @--keep-one-failed@ flag is present or not
  -> Hapistrano ()
dropOldReleases deployPath n keepOneFailed = do
  failedReleases <- releasesWithState Fail deployPath
  when (keepOneFailed && length failedReleases > 1) $
    -- Remove every failed release except the most recent one
    forM_ (tail failedReleases) $ \release -> do
      rpath <- releasePath deployPath release Nothing
      exec (Rm rpath) Nothing
  dreleases <- deployedReleases deployPath
  forM_ (genericDrop n dreleases) $ \release -> do
    rpath <- releasePath deployPath release Nothing
    exec (Rm rpath) Nothing

-- | Play the given script switching to directory of given release.

playScript
  :: Path Abs Dir         -- ^ Deploy path
  -> Release              -- ^ Release identifier
  -> Maybe (Path Rel Dir) -- ^ Working directory
  -> ExecutionMode        -- ^ Execution mode
  -> [BuildCommand]       -- ^ Commands to execute
  -> Hapistrano ()
playScript deployDir release mWorkingDir executionMode cmds = do
  rpath <- releasePath deployDir release mWorkingDir
  forM_ (filter byTarget cmds) (flip execWithInheritStdout (Just release) . Cd rpath)
  where
    byTarget BuildCommand{..} = executionMode == LeadTarget || buildCommandExecutionMode == AllTargets

-- | Plays the given script on your machine locally.

playScriptLocally :: [GenericCommand] ->  Hapistrano ()
playScriptLocally cmds =
  local
    (\c ->
        c
        { configSshOptions = Nothing
        }) $
  forM_ cmds $ flip execWithInheritStdout Nothing

-- | Create a file with an initial config file by getting information from the
-- user.
initConfig :: IO String -> IO ()
initConfig getLine' = do
  configFilePath <- (FilePath.</> "hap.yml") <$> Directory.getCurrentDirectory
  alreadyExisting <- Directory.doesFileExist configFilePath
  when alreadyExisting $ do
    T.hPutStrLn stderr "'hap.yml' already exists"
    exitFailure
  putStrLn "Creating 'hap.yml'"
  defaults <- defaultInitTemplateConfig
  let prompt :: Read a => T.Text -> a -> IO a
      prompt title d = do
        T.putStrLn $ title <> "?: "
        x <- getLine'
        return $
          if null x
            then d
            else read x
      prompt' :: Read a => T.Text -> (InitTemplateConfig -> T.Text) -> (InitTemplateConfig -> a) -> IO a
      prompt' title f fd = prompt (title <> " (default: " <> f defaults <> ")") (fd defaults)

  let yesNo :: a -> a -> T.Text -> a
      yesNo t f x = if x == "y" then t else f

  config <-
    InitTemplateConfig
      <$> prompt' "repo" repo repo
      <*> prompt' "revision" revision revision
      <*> prompt' "host" host host
      <*> prompt' "port" (T.pack . show . port) port
      <*> return (buildScript defaults)
      <*> fmap (yesNo (restartCommand defaults) Nothing) (prompt' "Include restart command" (const "Y/n") (const "y"))

  Yaml.encodeFile configFilePath config
  putStrLn $ "Configuration written at " <> configFilePath

----------------------------------------------------------------------------
-- Helpers

-- | Ensure that necessary directories exist. Idempotent.

setupDirs
  :: Path Abs Dir      -- ^ Deploy path
  -> Hapistrano ()
setupDirs deployPath = do
  (flip exec Nothing . MkDir . releasesPath)  deployPath
  (flip exec Nothing . MkDir . cacheRepoPath) deployPath
  (flip exec Nothing . MkDir . sharedPath)    deployPath

-- | Ensure that the specified repo is cloned and checked out on the given
-- revision. Idempotent.

ensureCacheInPlace
  :: String            -- ^ Repo URL
  -> Path Abs Dir      -- ^ Deploy path
  -> Maybe Release     -- ^ Release that was being attempted, if it was defined
  -> Hapistrano ()
ensureCacheInPlace repo deployPath maybeRelease = do
  let cpath = cacheRepoPath deployPath
      refs  = cpath </> $(mkRelDir "refs")
  exists <- (exec (Ls refs) Nothing >> return True)
    `catch` (\(_ :: HapistranoException)  -> return False)
  unless exists $
    exec (GitClone True (Left repo) cpath) maybeRelease
  exec (Cd cpath (GitSetOrigin repo)) maybeRelease
  exec (Cd cpath (GitFetch "origin")) maybeRelease -- TODO store this in task description?

-- | Create a new release identifier based on current timestamp.

newRelease :: ReleaseFormat -> Hapistrano Release
newRelease releaseFormat =
  mkRelease releaseFormat <$> liftIO getCurrentTime

-- | Clone the repository to create the specified 'Release'.

cloneToRelease
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ 'Release' to create
  -> Hapistrano ()
cloneToRelease deployPath release = do
  rpath <- releasePath deployPath release Nothing
  let cpath = cacheRepoPath deployPath
  exec (GitClone False (Right cpath) rpath) (Just release)

-- | Set the release to the correct revision by checking out a branch or
-- a commit.

setReleaseRevision
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ 'Release' to checkout
  -> String            -- ^ Revision to checkout
  -> Hapistrano ()
setReleaseRevision deployPath release revision = do
  rpath <- releasePath deployPath release Nothing
  exec (Cd rpath (GitCheckout revision)) (Just release)

-- | Return a list of all currently deployed releases sorted newest first.

deployedReleases
  :: Path Abs Dir      -- ^ Deploy path
  -> Hapistrano [Release]
deployedReleases deployPath = do
  let rpath = releasesPath deployPath
  xs <- exec (Find 1 rpath :: Find Dir) Nothing
  ps <- stripDirs rpath (filter (/= rpath) xs)
  (return . sortOn Down . mapMaybe parseRelease)
    (dropWhileEnd (== '/') . fromRelDir <$> ps)

-- | Return a list of successfully completed releases sorted newest first.

releasesWithState
  :: DeployState       -- ^ Selector for failed or successful releases
  -> Path Abs Dir      -- ^ Deploy path
  -> Hapistrano [Release]
releasesWithState selectedState deployPath = do
  releases <- deployedReleases deployPath
  filterM (
    fmap ((\bool -> if selectedState == Success then bool else not bool) . stateToBool)
     . deployState deployPath Nothing
    ) releases
  where
    stateToBool :: DeployState -> Bool
    stateToBool Fail = False
    stateToBool _    = True

----------------------------------------------------------------------------
-- Path helpers

-- | Return the full path to the directory containing all of the release
-- builds.

releasesPath
  :: Path Abs Dir      -- ^ Deploy path
  -> Path Abs Dir
releasesPath deployPath = deployPath </> $(mkRelDir "releases")

-- | Return the full path to the directory containing the shared files/directories.

sharedPath
  :: Path Abs Dir      -- ^ Deploy path
  -> Path Abs Dir
sharedPath deployPath = deployPath </> $(mkRelDir "shared")

-- | Link something (file or directory) from the {deploy_path}/shared/ directory
-- to a release

linkToShared
  :: TargetSystem -- ^ System to deploy
  -> Path Abs Dir -- ^ Release path
  -> Path Abs Dir -- ^ Deploy path
  -> FilePath     -- ^ Thing to link in share
  -> Maybe Release -- ^ Release that was being attempted, if it was defined
  -> Hapistrano ()
linkToShared configTargetSystem rpath configDeployPath thingToLink maybeRelease = do
  destPath <- parseRelFile thingToLink
  let dpath = rpath </> destPath
      sharedPath' = sharedPath configDeployPath </> destPath
  exec (Ln configTargetSystem sharedPath' dpath) maybeRelease

-- | Construct path to a particular 'Release'.

releasePath
  :: Path Abs Dir         -- ^ Deploy path
  -> Release              -- ^ 'Release' identifier
  -> Maybe (Path Rel Dir) -- ^ Working directory
  -> Hapistrano (Path Abs Dir)
releasePath deployPath release mWorkingDir =
  let rendered = renderRelease release
  in case parseRelDir rendered of
    Nothing    -> failWith 1 (Just $ "Could not append path: " ++ rendered) (Just release)
    Just rpath ->
      return $ case mWorkingDir of
        Nothing         -> releasesPath deployPath </> rpath
        Just workingDir -> releasesPath deployPath </> rpath </> workingDir

-- | Return the full path to the git repo used for cache purposes on the
-- target host filesystem.

cacheRepoPath
  :: Path Abs Dir      -- ^ Deploy path
  -> Path Abs Dir
cacheRepoPath deployPath = deployPath </> $(mkRelDir "repo")

-- | Get full path to current symlink.

currentSymlinkPath
  :: Path Abs Dir      -- ^ Deploy path
  -> Path Abs File
currentSymlinkPath deployPath = deployPath </> $(mkRelFile "current")

-- | Get full path to temp symlink.

tempSymlinkPath
  :: Path Abs Dir      -- ^ Deploy path
  -> Path Abs File
tempSymlinkPath deployPath = deployPath </> $(mkRelFile "current_tmp")

-- | Checks if a release was deployed properly or not
-- by looking into the @.hapistrano_deploy_state@ file.
-- If the file doesn't exist or the contents are anything other than
-- 'Fail' or 'Success', it returns 'Nothing'.

deployState
  :: Path Abs Dir -- ^ Deploy path
  -> Maybe (Path Rel Dir) -- ^ Working directory
  -> Release -- ^ 'Release' identifier
  -> Hapistrano DeployState -- ^ Whether the release was deployed successfully or not
deployState deployPath mWorkingDir release = do
  parseStatePath <- parseRelFile deployStateFilename
  actualReleasePath <- releasePath deployPath release mWorkingDir
  let stateFilePath = actualReleasePath </> parseStatePath
  doesExist <- exec (CheckExists stateFilePath) (Just release)
  if doesExist then do
    deployStateContents <- exec (Cat stateFilePath) (Just release)
    return $ (fromMaybe Unknown . readMaybe) deployStateContents
  else return Unknown

stripDirs :: Path Abs Dir -> [Path Abs t] -> Hapistrano [Path Rel t]
stripDirs path =
#if MIN_VERSION_path(0,6,0)
  mapM (stripProperPrefix path)
#else
  mapM (stripDir path)
#endif
