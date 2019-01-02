{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config
  ( Config (..)
  , CopyThing (..) )
where

import           Data.Aeson
import           Data.Function              (on)
import           Data.List                  (nubBy)
import           Data.Maybe                 (maybeToList)
import           Data.Yaml
import           Numeric.Natural
import           Path
import           System.Hapistrano.Commands
import           System.Hapistrano.Types    (ReleaseFormat (..),
                                             TargetSystem (..))

-- | Hapistrano configuration typically loaded from @hap.yaml@ file.

data Config = Config
  { configDeployPath     :: !(Path Abs Dir)
    -- ^ Top-level deploy directory on target machine
  , configHosts          :: ![(String, Word)]
    -- ^ Hosts\/ports to deploy to. If empty, localhost will be assumed.
  , configRepo           :: !String
    -- ^ Location of repository that contains the source code to deploy
  , configRevision       :: !String
    -- ^ Revision to use
  , configRestartCommand :: !(Maybe GenericCommand)
    -- ^ The command to execute when switching to a different release
    -- (usually after a deploy or rollback).
  , configBuildScript    :: !(Maybe [GenericCommand])
    -- ^ Build script to execute to build the project
  , configCopyFiles      :: ![CopyThing]
    -- ^ Collection of files to copy over to target machine before building
  , configCopyDirs       :: ![CopyThing]
    -- ^ Collection of directories to copy over to target machine before building
  , configLinkedFiles      :: ![FilePath]
    -- ^ Collection of files to link from each release to _shared_
  , configLinkedDirs       :: ![FilePath]
    -- ^ Collection of directories to link from each release to _shared_
  , configVcAction       :: !Bool
  -- ^ Perform version control related actions. By default, it's assumed to be True.
  , configRunLocally     :: !(Maybe [GenericCommand])
  -- ^ Perform a series of commands on the local machine before communication
  -- with target server starts
  , configTargetSystem   :: !TargetSystem
  -- ^ Optional parameter to specify the target system. It's GNU/Linux by
  -- default
  , configReleaseFormat  :: !(Maybe ReleaseFormat)
  -- ^ The release timestamp format, the '--release-format' argument passed via
  -- the CLI takes precedence over this value. If neither CLI or configuration
  -- file value is specified, it defaults to short
  , configKeepReleases   :: !(Maybe Natural)
  -- ^ The number of releases to keep, the '--keep-releases' argument passed via
  -- the CLI takes precedence over this value. If neither CLI or configuration
  -- file value is specified, it defaults to 5
  } deriving (Eq, Ord, Show)

-- | Information about source and destination locations of a file\/directory
-- to copy.

data CopyThing = CopyThing FilePath FilePath
  deriving (Eq, Ord, Show)

instance FromJSON Config where
  parseJSON = withObject "Hapistrano configuration" $ \o -> do
    configDeployPath <- o .: "deploy_path"
    let grabPort m = m .:? "port" .!= 22
    host             <- o .:? "host"
    port             <- grabPort o
    hs               <- (o .:? "targets" .!= []) >>= mapM (\m -> do
      host' <- m .: "host"
      port' <- grabPort m
      return (host', port'))
    let configHosts = nubBy ((==) `on` fst)
          (maybeToList ((,) <$> host <*> pure port) ++ hs)
    configRepo       <- o .: "repo"
    configRevision   <- o .: "revision"
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
