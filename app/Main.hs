{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified System.Hapistrano as Hap
import Control.Monad (void)
import System.Environment.Compat (lookupEnv)

import System.Hapistrano (ReleaseFormat(..))

import Control.Applicative (pure, (<*>))
import qualified System.Exit.Compat as Exit

import Options

import Paths_hapistrano (version)
import Data.Version (showVersion)

-- | Rolls back to previous release.
rollback :: Hap.Config -> IO ()
rollback cfg =
  Hap.runRC errorHandler successHandler cfg $ do

    _ <- Hap.rollback
    void Hap.restartServerCommand

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler

-- | Deploys the current release with Config options.
deploy :: Hap.Config -> IO ()
deploy cfg =
  Hap.runRC errorHandler successHandler cfg $ do
    _ <- Hap.pushRelease >>= Hap.runBuild >>= Hap.activateRelease

    void Hap.restartServerCommand

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler

-- | Retrieves the configuration from environment variables.
configFromEnv :: IO Hap.Config
configFromEnv = do
  maybeDeployPath <- lookupEnv "DEPLOY_PATH"
  maybeRepository <- lookupEnv "REPOSITORY"
  maybeRevision <- lookupEnv "REVISION"

  deployPath <- maybe (Exit.die (noEnv "DEPLOY_PATH")) return maybeDeployPath
  repository <- maybe (Exit.die (noEnv "REPOSITORY")) return maybeRepository
  revision <- maybe (Exit.die (noEnv "REVISION")) return maybeRevision

  host           <- lookupEnv "HOST"
  buildScript    <- lookupEnv "BUILD_SCRIPT"
  restartCommand <- lookupEnv "RESTART_COMMAND"

  return Hap.Config { Hap.deployPath     = deployPath
                    , Hap.host           = host
                    , Hap.releaseFormat  = Short
                    , Hap.repository     = repository
                    , Hap.revision       = revision
                    , Hap.buildScript    = buildScript
                    , Hap.restartCommand = restartCommand
                    }
  where
    noEnv env = env ++ " environment variable does not exist"

main :: IO ()
main = execParser (info (helper <*> opts) hapistranoDesc) >>= runOption

runOption :: Option -> IO ()
runOption (Command command) = runCommand command
runOption (Flag flag) = runFlag flag

runCommand :: Command -> IO ()
runCommand Deploy = configFromEnv >>= deploy
runCommand Rollback = configFromEnv >>= rollback

runFlag :: Flag -> IO ()
runFlag Version = printVersion

printVersion :: IO ()
printVersion = putStrLn $ "Hapistrano " ++ showVersion version

