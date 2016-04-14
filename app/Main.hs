{-# LANGUAGE CPP #-}

module Main (main) where

import qualified System.Hapistrano as Hap
import Control.Monad (void)
import System.Environment.Compat (lookupEnv)

import System.Hapistrano (ReleaseFormat(..))

import System.Exit

import Options

import Paths_hapistrano (version)
import Data.Version (showVersion)

import qualified Text.Read as Read

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import System.IO

die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure
#endif

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

  deployPath <- maybe (die (noEnv "DEPLOY_PATH")) return maybeDeployPath
  repository <- maybe (die (noEnv "REPOSITORY")) return maybeRepository
  revision <- maybe (die (noEnv "REVISION")) return maybeRevision

  port           <- lookupEnv "PORT"
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
                    , Hap.port           = parsePort port
                    }
  where
    noEnv env = env ++ " environment variable does not exist"
    parsePort maybePort = maybePort >>= Read.readMaybe

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
