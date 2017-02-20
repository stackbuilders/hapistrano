{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config
  ( Config (..) )
where

import Data.Aeson
import Data.Yaml
import Path
import System.Hapistrano.Commands

-- | Hapistrano configuration typically loaded from @hap.yaml@ file.

data Config = Config
  { configDeployPath :: !(Path Abs Dir)
    -- ^ Top-level deploy directory on target machine
  , configHost :: !(Maybe String)
    -- ^ Host to deploy to. If missing, localhost will be assumed.
  , configPort :: !Word
    -- ^ SSH port number to use, may be omitted
  , configRepo :: !String
    -- ^ Location of repository that contains the source code to deploy
  , configRevision :: !String
    -- ^ Revision to use
  , configRestartCommand :: !(Maybe GenericCommand)
    -- ^ The command to execute when switching to a different release
    -- (usually after a deploy or rollback).
  , configBuildScript :: !(Maybe [GenericCommand])
    -- ^ Build script to execute to build the project
  } deriving (Eq, Ord, Show)

instance FromJSON Config where
  parseJSON = withObject "Hapistrano configuration" $ \o -> do
    configDeployPath <- o .: "deploy_path"
    configHost       <- o .:? "host"
    configPort       <- o .:? "port" .!= 22
    configRepo       <- o .: "repo"
    configRevision   <- o .: "revision"
    configRestartCommand <- (o .:? "restart_command") >>=
      maybe (return Nothing) (fmap Just . mkCmd)
    configBuildScript <- o .:? "build_script" >>=
      maybe (return Nothing) (fmap Just . mapM mkCmd)
    return Config {..}

mkCmd :: String -> Parser GenericCommand
mkCmd raw =
  case mkGenericCommand raw of
    Nothing -> fail "invalid restart command"
    Just cmd -> return cmd
