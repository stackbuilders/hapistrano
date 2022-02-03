-- |
-- Module      :  System.Config
-- Copyright   :  © 2015-Present Stack Builders
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for types and functions related to the configuration
-- of the Hapistrano tool.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Hapistrano.Config
  ( Config (..)
  , CopyThing (..)
  , Target (..)
  , deployStateFilename)
where

import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Function              (on)
import           Data.List                  (nubBy)
import           Data.Maybe                 (maybeToList)
import           Data.Yaml
import           Numeric.Natural
import           Path
import           System.Hapistrano.Commands
import           System.Hapistrano.Types    (ReleaseFormat (..), Shell (..),
                                             Source (..), TargetSystem (..))

-- | Hapistrano configuration typically loaded from @hap.yaml@ file.

data Config = Config
  { configDeployPath     :: !(Path Abs Dir)
    -- ^ Top-level deploy directory on target machine
  , configHosts          :: ![Target]
    -- ^ Hosts\/ports\/shell\/ssh args to deploy to. If empty, localhost will be assumed.
  , configSource         :: !Source
    -- ^ Location of the 'Source' that contains the code to deploy
  , configRestartCommand :: !(Maybe GenericCommand)
    -- ^ The command to execute when switching to a different release
    -- (usually after a deploy or rollback).
  , configBuildScript    :: !(Maybe [GenericCommand])
    -- ^ Build script to execute to build the project
  , configCopyFiles      :: ![CopyThing]
    -- ^ Collection of files to copy over to target machine before building
  , configCopyDirs       :: ![CopyThing]
    -- ^ Collection of directories to copy over to target machine before building
  , configLinkedFiles    :: ![FilePath]
    -- ^ Collection of files to link from each release to _shared_
  , configLinkedDirs     :: ![FilePath]
    -- ^ Collection of directories to link from each release to _shared_
  , configVcAction       :: !Bool
  -- ^ Perform version control related actions. By default, it's assumed to be `True`.
  , configRunLocally     :: !(Maybe [GenericCommand])
  -- ^ Perform a series of commands on the local machine before communication
  -- with target server starts
  , configTargetSystem   :: !TargetSystem
  -- ^ Optional parameter to specify the target system. It's GNU/Linux by
  -- default
  , configReleaseFormat  :: !(Maybe ReleaseFormat)
  -- ^ The release timestamp format, the @--release-format@ argument passed via
  -- the CLI takes precedence over this value. If neither CLI or configuration
  -- file value is specified, it defaults to short
  , configKeepReleases   :: !(Maybe Natural)
  -- ^ The number of releases to keep, the @--keep-releases@ argument passed via
  -- the CLI takes precedence over this value. If neither CLI or configuration
  -- file value is specified, it defaults to 5
  , configKeepOneFailed  :: !Bool
  -- ^ Specifies whether to keep all failed releases along with the successful releases
  -- or just the latest failed (at least this one should be kept for debugging purposes).
  -- The @--keep-one-failed@ argument passed via the CLI takes precedence over this value.
  -- If neither CLI or configuration file value is specified, it defaults to `False`
  -- (i.e. keep all failed releases).
  , configWorkingDir :: !(Maybe (Path Rel Dir))

  , configMaintenanceFilePath :: !(Path Abs  Dir)
  --
  , configMaintenanceFileName :: !(String)
  -- TODO: Change FileName type
  } deriving (Eq, Ord, Show)

-- | Information about source and destination locations of a file\/directory
-- to copy.

data CopyThing = CopyThing FilePath FilePath
  deriving (Eq, Ord, Show)

-- | Datatype that holds information about the target host.

data Target =
  Target
    { targetHost    :: String
    , targetPort    :: Word
    , targetShell   :: Shell
    , targetSshArgs :: [String]
    } deriving (Eq, Ord, Show)

instance FromJSON Config where
  parseJSON = withObject "Hapistrano configuration" $ \o -> do
    configDeployPath <- o .: "deploy_path"
    let grabPort m = m .:? "port" .!= 22
        grabShell m = m .:? "shell" .!= Bash
        grabSshArgs m = m .:? "ssh_args" .!= []
    host             <- o .:? "host"
    port             <- grabPort o
    shell            <- grabShell o
    sshArgs          <- grabSshArgs o
    hs               <- (o .:? "targets" .!= []) >>= mapM (\m ->
      Target
        <$> m .: "host"
        <*> grabPort m
        <*> grabShell m
        <*> grabSshArgs m)
    let first Target{} = host
        configHosts = nubBy ((==) `on` first)
          (maybeToList (Target <$> host <*> pure port <*> pure shell <*> pure sshArgs) ++ hs)
        source m =
              GitRepository <$> m .: "repo" <*> m .: "revision"
          <|> LocalDirectory <$> m .: "local_directory"
    configSource  <- source o
    configRestartCommand <- (o .:? "restart_command") >>=
      maybe (return Nothing) (fmap Just . mkCmd)
    configBuildScript <- o .:? "build_script" >>=
      maybe (return Nothing) (fmap Just . mapM mkCmd)
    configCopyFiles  <- o .:? "copy_files" .!= []
    configCopyDirs   <- o .:? "copy_dirs"  .!= []
    configLinkedFiles <- o .:? "linked_files" .!= []
    configLinkedDirs  <- o .:? "linked_dirs"  .!= []
    configVcAction    <- o .:? "vc_action" .!= True
    configRunLocally  <- o .:? "run_locally" >>=
      maybe (return Nothing) (fmap Just . mapM mkCmd)
    configTargetSystem <- o .:? "linux" .!= GNULinux
    configReleaseFormat <- o .:? "release_format"
    configKeepReleases <- o .:? "keep_releases"
    configKeepOneFailed <- o .:? "keep_one_failed" .!= False
    configWorkingDir <- o .:? "working_directory"
    configMaintenanceFilePath <- o .:? "maintenance_directory" .!= configDeployPath <> undefined
    configMaintenanceFileName <- o .:? "maintenance_filename" .!= ("maintenance.html"::String)
    return Config {..}

instance FromJSON CopyThing where
  parseJSON = withObject "src and dest of a thing to copy" $ \o ->
    CopyThing <$> (o .: "src") <*> (o .: "dest")

instance FromJSON TargetSystem where
  parseJSON = withBool "linux" $
    pure . \case
      True  -> GNULinux
      False -> BSD

mkCmd :: String -> Parser GenericCommand
mkCmd raw =
  case mkGenericCommand raw of
    Nothing  -> fail "invalid restart command"
    Just cmd -> return cmd

-- | Constant with the name of the file used to store
-- the deployment state information.

deployStateFilename :: String
deployStateFilename = ".hapistrano_deploy_state"
