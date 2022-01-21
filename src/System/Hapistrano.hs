-- |
-- Module      :  System.Hapistrano
-- Copyright   :  © 2015-Present Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Juan Paucar <jpaucar@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for creating reliable deploy processes for Haskell applications.
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module System.Hapistrano
  ( runHapistrano
  , pushRelease
  , pushReleaseWithoutVc
  , registerReleaseAsComplete
  , activateRelease
  , linkToShared
  , createHapistranoDeployState
  , rollback
  , dropOldReleases
  , playScript
  , playScriptLocally
    -- * Path helpers
  , releasePath
  , sharedPath
  , currentSymlinkPath
  , tempSymlinkPath
  , ctokenPath )
where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader       (local, runReaderT)
import           Data.List                  (dropWhileEnd, genericDrop, sortOn)
import           Data.Maybe                 (mapMaybe)
import           Data.Ord                   (Down (..))
import           Data.Time
import           Numeric.Natural
import           Path
import           System.Hapistrano.Commands
import           System.Hapistrano.Core
import           System.Hapistrano.Types
import qualified System.Hapistrano.Config as C

----------------------------------------------------------------------------

-- | Run the 'Hapistrano' monad. The monad hosts 'exec' actions.
runHapistrano ::
     MonadIO m
  => Maybe SshOptions -- ^ SSH options to use or 'Nothing' if we run locally
  -> Shell -- ^ Shell to run commands
  -> (OutputDest -> String -> IO ()) -- ^ How to print messages
  -> Opts -- ^ CLI options
  -> C.Config -- ^ Config file options
  -> Hapistrano a -- ^ The computation to run
  -> m (Either Int a) -- ^ Status code in 'Left' on failure, result in
              -- 'Right' on success
runHapistrano sshOptions shell' printFnc Opts{..} C.Config{..} m =
  liftIO $ do
    let config =
          Config
            { configSshOptions = sshOptions
            , configShellOptions = shell'
            , configPrint = printFnc
            }
    r <- runReaderT (runExceptT $ m `catchError` failStateAndThrow) config
    case r of
      Left (Failure n msg) -> do
        forM_ msg (printFnc StderrDest)
        return (Left n)
      Right x -> return (Right x)
    where
      failStateAndThrow e = do
        let task rf = Task { taskDeployPath    = configDeployPath
                     , taskSource        = configSource
                     , taskReleaseFormat = rf }
        case optsCommand of
            Deploy cliReleaseFormat _ _ -> do
              let releaseFormat = fromMaybeReleaseFormat cliReleaseFormat configReleaseFormat
              release <- if configVcAction
                    then pushRelease (task releaseFormat)
                    else pushReleaseWithoutVc (task releaseFormat)
              createHapistranoDeployState configDeployPath release Fail >> throwError e
            _ -> throwError e

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
      ensureCacheInPlace gitRepositoryURL taskDeployPath
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

-- | Create a file-token that will tell rollback function that this release
-- should be considered successfully compiled\/completed.

registerReleaseAsComplete
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ Release identifier to activate
  -> Hapistrano ()
registerReleaseAsComplete deployPath release = do
  cpath <- ctokenPath deployPath release
  exec (Touch cpath)

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
  exec (Ln ts rpath tpath) -- create a symlink for the new candidate
  exec (Mv ts tpath cpath) -- atomically replace the symlink

-- | Creates the file @.hapistrano__state@ containing
-- @fail@ or @success@ depending on how the deployment ended.

createHapistranoDeployState
  :: Path Abs Dir -- ^ Deploy path
  -> Release -- ^ Release being deployed
  -> DeployState -- ^ Indicates how the deployment went
  -> Hapistrano ()
createHapistranoDeployState deployPath release deployState = do
  parseStatePath <- parseRelFile ".hapistrano_deploy_state"
  actualReleasePath <- releasePath deployPath release Nothing
  let stateFilePath = actualReleasePath </> parseStatePath
  exec (Touch stateFilePath) -- creates '.hapistrano_deploy_state'
  exec (BasicWrite stateFilePath $ show deployState) -- writes the deploy state to '.hapistrano_deploy_state'

-- | Activates one of already deployed releases.

rollback
  :: TargetSystem
  -> Path Abs Dir      -- ^ Deploy path
  -> Natural           -- ^ How many releases back to go, 0 re-activates current
  -> Hapistrano ()
rollback ts deployPath n = do
  crs <- completedReleases deployPath
  drs <- deployedReleases  deployPath
  -- NOTE If we don't have any completed releases, then perhaps the
  -- application was used with older versions of Hapistrano that did not
  -- have this functionality. We then fall back and use collection of “just”
  -- deployed releases.
  case genericDrop n (if null crs then drs else crs) of
    [] -> failWith 1 (Just "Could not find the requested release to rollback to.")
    (x:_) -> activateRelease ts deployPath x

-- | Remove older releases to avoid filling up the target host filesystem.

dropOldReleases
  :: Path Abs Dir      -- ^ Deploy path
  -> Natural           -- ^ How many releases to keep
  -> Hapistrano ()     -- ^ Deleted Releases
dropOldReleases deployPath n = do
  dreleases <- deployedReleases deployPath
  forM_ (genericDrop n dreleases) $ \release -> do
    rpath <- releasePath deployPath release Nothing
    exec (Rm rpath)
  creleases <- completedReleases deployPath
  forM_ (genericDrop n creleases) $ \release -> do
    cpath <- ctokenPath deployPath release
    exec (Rm cpath)

-- | Play the given script switching to directory of given release.

playScript
  :: Path Abs Dir         -- ^ Deploy path
  -> Release              -- ^ Release identifier
  -> Maybe (Path Rel Dir) -- ^ Working directory
  -> [GenericCommand]     -- ^ Commands to execute
  -> Hapistrano ()
playScript deployDir release mWorkingDir cmds = do
  rpath <- releasePath deployDir release mWorkingDir
  forM_ cmds (execWithInheritStdout . Cd rpath)

-- | Plays the given script on your machine locally.

playScriptLocally :: [GenericCommand] -> Hapistrano ()
playScriptLocally cmds =
  local
    (\c ->
        c
        { configSshOptions = Nothing
        }) $
  forM_ cmds execWithInheritStdout

----------------------------------------------------------------------------
-- Helpers

-- | Ensure that necessary directories exist. Idempotent.

setupDirs
  :: Path Abs Dir      -- ^ Deploy path
  -> Hapistrano ()
setupDirs deployPath = do
  (exec . MkDir . releasesPath)  deployPath
  (exec . MkDir . cacheRepoPath) deployPath
  (exec . MkDir . ctokensPath)   deployPath
  (exec . MkDir . sharedPath)    deployPath

-- | Ensure that the specified repo is cloned and checked out on the given
-- revision. Idempotent.

ensureCacheInPlace
  :: String            -- ^ Repo URL
  -> Path Abs Dir      -- ^ Deploy path
  -> Hapistrano ()
ensureCacheInPlace repo deployPath = do
  let cpath = cacheRepoPath deployPath
      refs  = cpath </> $(mkRelDir "refs")
  exists <- (exec (Ls refs) >> return True)
    `catchError` const (return False)
  unless exists $
    exec (GitClone True (Left repo) cpath)
  exec (Cd cpath (GitFetch "origin")) -- TODO store this in task description?

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
  exec (GitClone False (Right cpath) rpath)

-- | Set the release to the correct revision by checking out a branch or
-- a commit.

setReleaseRevision
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ 'Release' to checkout
  -> String            -- ^ Revision to checkout
  -> Hapistrano ()
setReleaseRevision deployPath release revision = do
  rpath <- releasePath deployPath release Nothing
  exec (Cd rpath (GitCheckout revision))

-- | Return a list of all currently deployed releases sorted newest first.

deployedReleases
  :: Path Abs Dir      -- ^ Deploy path
  -> Hapistrano [Release]
deployedReleases deployPath = do
  let rpath = releasesPath deployPath
  xs <- exec (Find 1 rpath :: Find Dir)
  ps <- stripDirs rpath (filter (/= rpath) xs)
  (return . sortOn Down . mapMaybe parseRelease)
    (dropWhileEnd (== '/') . fromRelDir <$> ps)

-- | Return a list of successfully completed releases sorted newest first.

completedReleases
  :: Path Abs Dir      -- ^ Deploy path
  -> Hapistrano [Release]
completedReleases deployPath = do
  let cpath = ctokensPath deployPath
  xs <- exec (Find 1 cpath :: Find File)
  ps <- stripDirs cpath xs
  (return . sortOn Down . mapMaybe parseRelease)
    (dropWhileEnd (== '/') . fromRelFile <$> ps)

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
  -> Hapistrano ()
linkToShared configTargetSystem rpath configDeployPath thingToLink = do
  destPath <- parseRelFile thingToLink
  let dpath = rpath </> destPath
      sharedPath' = sharedPath configDeployPath </> destPath
  exec $ Ln configTargetSystem sharedPath' dpath

-- | Construct path to a particular 'Release'.

releasePath
  :: Path Abs Dir         -- ^ Deploy path
  -> Release              -- ^ 'Release' identifier
  -> Maybe (Path Rel Dir) -- ^ Working directory
  -> Hapistrano (Path Abs Dir)
releasePath deployPath release mWorkingDir =
  let rendered = renderRelease release
  in case parseRelDir rendered of
    Nothing    -> failWith 1 (Just $ "Could not append path: " ++ rendered)
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

-- | Get path to the directory that contains tokens of build completion.

ctokensPath
  :: Path Abs Dir      -- ^ Deploy path
  -> Path Abs Dir
ctokensPath deployPath = deployPath </> $(mkRelDir "ctokens")

-- | Get path to completion token file for particular release.

ctokenPath
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ 'Release' identifier
  -> Hapistrano (Path Abs File)
ctokenPath deployPath release = do
  let rendered = renderRelease release
  case parseRelFile rendered of
    Nothing    -> failWith 1 (Just $ "Could not append path: " ++ rendered)
    Just rpath -> return (ctokensPath deployPath </> rpath)

stripDirs :: Path Abs Dir -> [Path Abs t] -> Hapistrano [Path Rel t]
stripDirs path =
#if MIN_VERSION_path(0,6,0)
  mapM (stripProperPrefix path)
#else
  mapM (stripDir path)
#endif
