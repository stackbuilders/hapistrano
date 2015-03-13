module Main where

import qualified System.Hapistrano as Hap
import Control.Monad (void)
import System.Environment (getArgs, getEnv)
import System.Environment.Compat (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import System.Hapistrano.Types (ReleaseFormat(..))

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
  deployPath     <- getEnv    "DEPLOY_PATH"
  repository     <- getEnv    "REPOSITORY"
  revision       <- getEnv    "REVISION"

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      hPutStrLn stderr
        "First argument must be either 'deploy' or 'rollback'."
      exitFailure

    arg1:_ -> do
      cfg <- configFromEnv

      case arg1 of
        "deploy"   -> deploy cfg
        "rollback" -> rollback cfg
        _ -> do
          hPutStrLn stderr $ "Invalid argument: " ++ arg1
          exitFailure
