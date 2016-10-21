{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified System.Hapistrano as Hap
import Control.Monad (void)
import System.Environment.Compat (lookupEnv)

import System.Hapistrano (ReleaseFormat(..))

import qualified Control.Monad as Monad
import qualified System.Exit.Compat as Exit

import Data.Monoid((<>))
import Options.Applicative(ParserPrefs(..))
import qualified Options.Applicative as Opt

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
main = do
  hapConfiguration <- configFromEnv
  Monad.join $ Opt.customExecParser prefs (Opt.info (opts hapConfiguration) Opt.idm)
    where
      opts hapConfig =
        Opt.helper
          Opt.<*> Opt.subparser (commands hapConfig)
          Opt.<|> Opt.flag' printVersion (Opt.long "version" <> Opt.short 'v' <> Opt.help "Diplay the version of Hapistrano")
      commands hapConfig =
        Opt.command "deploy" (Opt.info (Opt.pure $ deploy hapConfig) Opt.idm)
        <> Opt.command "rollback"  (Opt.info (Opt.pure $ rollback hapConfig) Opt.idm)

prefs :: ParserPrefs
prefs =
  Opt.defaultPrefs { prefShowHelpOnEmpty = True }

printVersion :: IO ()
printVersion = putStrLn $ "Hapistrano " ++ showVersion version

