-- |
-- Module      :  System.Hapistrano
-- Copyright   :  © 2015-2017 Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for creating reliable deploy processes for Haskell applications.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module System.Hapistrano
  ( pushRelease
  , registerReleaseAsComplete
  , activateRelease
  , rollback
  , dropOldReleases
  , playScript
    -- * Path helpers
  , releasePath
  , currentSymlinkPath
  , tempSymlinkPath
  , ctokenPath )
where

import Control.Monad
import Control.Monad.Except
import Data.List (genericDrop, dropWhileEnd, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing, Down (..))
import Data.Time
import Numeric.Natural
import Path
import System.Hapistrano.Commands
import System.Hapistrano.Core
import System.Hapistrano.Types

----------------------------------------------------------------------------
-- High-level functionality

-- | Perform basic setup for a project, making sure necessary directories
-- exist and pushing a new release directory with the SHA1 or branch
-- specified in the configuration. Return identifier of the pushed release.

pushRelease :: Task -> Hapistrano Release
pushRelease Task {..} = do
  setupDirs taskDeployPath
  ensureCacheInPlace taskRepository taskDeployPath
  release <- newRelease taskReleaseFormat
  cloneToRelease taskDeployPath release
  setReleaseRevision taskDeployPath release taskRevision
  return release

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
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ Release identifier to activate
  -> Hapistrano ()
activateRelease deployPath release = do
  rpath <- releasePath deployPath release
  let tpath = tempSymlinkPath deployPath
      cpath = currentSymlinkPath deployPath
  exec (Ln rpath tpath) -- create a symlink for the new candidate
  exec (Mv tpath cpath) -- atomically replace the symlink

-- | Activates one of already deployed releases.

rollback
  :: Path Abs Dir      -- ^ Deploy path
  -> Natural           -- ^ How many releases back to go, 0 re-activates current
  -> Hapistrano ()
rollback deployPath n = do
  crs <- completedReleases deployPath
  drs <- deployedReleases  deployPath
  -- NOTE If we don't have any completed releases, then perhaps the
  -- application was used with older versions of Hapistrano that did not
  -- have this functionality. We then fall back and use collection of “just”
  -- deployed releases.
  case genericDrop n (if null crs then drs else crs) of
    [] -> failWith 1 (Just "Could not find the requested release to rollback to.")
    (x:_) -> activateRelease deployPath x

-- | Remove older releases to avoid filling up the target host filesystem.

dropOldReleases
  :: Path Abs Dir      -- ^ Deploy path
  -> Natural           -- ^ How many releases to keep
  -> Hapistrano ()     -- ^ Deleted Releases
dropOldReleases deployPath n = do
  releases <- deployedReleases deployPath
  forM_ (genericDrop n releases) $ \release -> do
    rpath <- releasePath deployPath release
    exec (Rm rpath)

-- | Play the given script switching to diroctory of given release.

playScript
  :: [GenericCommand] -- ^ Commands to execute
  -> Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ Release identifier
  -> Hapistrano ()
playScript cmds deployDir release = do
  rpath <- releasePath deployDir release
  forM_ cmds (exec . Cd rpath)

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

-- | Create a new realese identifier based on current timestamp.

newRelease :: ReleaseFormat -> Hapistrano Release
newRelease releaseFormat =
  mkRelease releaseFormat <$> liftIO getCurrentTime

-- | Clone the repository to create the specified 'Release'.

cloneToRelease
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ 'Release' to create
  -> Hapistrano ()
cloneToRelease deployPath release = do
  rpath <- releasePath deployPath release
  let cpath = cacheRepoPath deployPath
  exec (GitClone False (Right cpath) rpath)

-- | Set the release to the correct revision by resetting the head of the
-- git repo.

setReleaseRevision
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ 'Release' to reset
  -> String            -- ^ Revision to reset to
  -> Hapistrano ()
setReleaseRevision deployPath release revision = do
  rpath <- releasePath deployPath release
  exec (Cd rpath (GitReset revision))

-- | Return a list of all currently deployed releases sorted newest first.

deployedReleases
  :: Path Abs Dir      -- ^ Deploy path
  -> Hapistrano [Release]
deployedReleases deployPath = do
  let rpath = releasesPath deployPath
  xs <- exec (Find 1 rpath :: Find Dir)
  ps <- mapM (stripDir rpath) (filter (/= rpath) xs)
  (return . sortBy (comparing Down) . mapMaybe parseRelease)
    (dropWhileEnd (== '/') . fromRelDir <$> ps)

-- | Return a list of successfully completed releases sorted newest first.

completedReleases
  :: Path Abs Dir      -- ^ Deploy path
  -> Hapistrano [Release]
completedReleases deployPath = do
  let cpath = ctokensPath deployPath
  xs <- exec (Find 1 cpath :: Find File)
  ps <- mapM (stripDir cpath) xs
  (return . sortBy (comparing Down) . mapMaybe parseRelease)
    (dropWhileEnd (== '/') . fromRelFile <$> ps)

----------------------------------------------------------------------------
-- Path helpers

-- | Return the full path to the directory containing all of the release
-- builds.

releasesPath
  :: Path Abs Dir      -- ^ Deploy path
  -> Path Abs Dir
releasesPath deployPath = deployPath </> $(mkRelDir "releases")

-- | Construct path to a particular 'Release'.

releasePath
  :: Path Abs Dir      -- ^ Deploy path
  -> Release           -- ^ 'Release' identifier
  -> Hapistrano (Path Abs Dir)
releasePath deployPath release = do
  let rendered = renderRelease release
  case parseRelDir rendered of
    Nothing -> failWith 1 (Just $ "Could not append path: " ++ rendered)
    Just rpath -> return (releasesPath deployPath </> rpath)

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
    Nothing -> failWith 1 (Just $ "Could not append path: " ++ rendered)
    Just rpath -> return (ctokensPath deployPath </> rpath)
