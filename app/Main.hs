{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified System.Hapistrano as Hap
import Control.Monad (join, void)
import System.Environment.Compat (lookupEnv)

import System.Hapistrano (ReleaseFormat(..))

import Control.Applicative (pure, (<*>))
import qualified System.Exit.Compat as Exit

import Data.Monoid ((<>))
import Options.Applicative
  ( CommandFields
  , InfoMod
  , Mod
  , command
  , header
  , help
  , helper
  , execParser
  , subparser
  , flag'
  , footer
  , info
  , long
  , progDesc
  , fullDesc
  , short
  , (<|>))

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
main =
  join $ execParser (info (helper <*> opts) hapistranoDesc)
    where
      opts =
        subparser commands
        <|> flag' printVersion (long "version" <> short 'v' <> help "Diplay the version of Hapistrano")
      commands =
        addCommand deployCommand
        <> addCommand rollbackCommand

hapistranoDesc :: InfoMod a
hapistranoDesc =
  fullDesc
    <> header "Hapistrano - A deployment library for Haskell applications"
    <> progDesc "Deploy tool for Haskell applications"
    <> footer "Run 'hap -h' for available commands"

data Command =
  Command
    { name :: String
    , effect :: IO ()
    , description :: String
    }

deployCommand :: Command
deployCommand =
  Command
    { name = "deploy"
    , effect = configFromEnv >>= deploy
    , description =  "Deploys the current release with the configure options"
    }

rollbackCommand :: Command
rollbackCommand =
  Command
    { name = "rollback"
    , effect = configFromEnv >>= rollback
    , description = "Rolls back to the previous release"
    }

addCommand :: Command -> Mod CommandFields (IO ())
addCommand Command{..} =
  command name (info (pure effect) (progDesc description))

printVersion :: IO ()
printVersion = putStrLn $ "Hapistrano " ++ showVersion version

