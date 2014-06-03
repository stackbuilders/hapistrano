module Main where

import qualified Hapistrano as Hap
import Control.Monad (void)
import System.Environment (getArgs, getEnv, lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(liftIO))

-- | Rolls back to previous release.
rollback :: Hap.Config -> IO ()
rollback cfg =
  Hap.runRC errorHandler successHandler (Hap.initialState cfg) $ do
    Hap.restartServerCommand
    void Hap.rollback

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler

-- | Deploys the current release with Config options.
deploy :: Hap.Config -> IO ()
deploy cfg =
  Hap.runRC errorHandler successHandler (Hap.initialState cfg) $ do
    Hap.pushRelease

    case Hap._buildScript cfg of
      Nothing ->
        return $
        putStrLn "No build file given in BUILD_SCRIPT, skipping build step."

      Just scr -> do
        fl <- liftIO $ readFile scr
        let commands = lines fl
        Hap.buildRelease commands
        return $ putStrLn "Done with build, activating release..."

    Hap.restartServerCommand

    void Hap.activateRelease

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler

-- | Retrieves the configuration from environment variables.
configFromEnv :: IO Hap.Config
configFromEnv = do
  deployPath     <- getEnv "DEPLOY_PATH"
  host           <- getEnv "HOST"
  repository     <- getEnv "REPOSITORY"
  revision       <- getEnv "REVISION"
  buildScript    <- lookupEnv "BUILD_SCRIPT"
  restartCommand <- lookupEnv "RESTART_COMMAND"

  return Hap.Config { Hap._deployPath     = deployPath
                    , Hap._host           = host
                    , Hap._repository     = repository
                    , Hap._revision       = revision
                    , Hap._buildScript    = buildScript
                    , Hap._restartCommand = restartCommand
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
