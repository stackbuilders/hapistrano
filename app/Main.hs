{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified System.Hapistrano as Hap
import Control.Monad (void)
import System.Environment.Compat (lookupEnv)

import System.Hapistrano (ReleaseFormat(..))

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.Exit.Compat as Exit
import qualified System.IO as IO

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

  host           <- fmap parseHostWithPort $ lookupEnv "HOST"
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

data HapCommand
  = HapDeploy
  | HapRollback
  deriving Show

parseHapCommand :: String -> Maybe HapCommand
parseHapCommand "deploy" = Just HapDeploy
parseHapCommand "rollback" = Just HapRollback
parseHapCommand _ = Nothing

data HapOptions =
  HapOptions
    { hapCommand :: Maybe HapCommand
    , hapHelp :: Bool
    }
  deriving Show

defaultHapOptions :: HapOptions
defaultHapOptions =
  HapOptions
    { hapCommand = Nothing
    , hapHelp = False
    }

hapOptionDescriptions :: [GetOpt.OptDescr (HapOptions -> HapOptions)]
hapOptionDescriptions =
  [ GetOpt.Option
      ['h']
      ["help"]
      (GetOpt.NoArg (\hapOptions -> hapOptions { hapHelp = True }))
      "Show this help text"

  ]

parseHapOptions :: [String] -> Either String HapOptions
parseHapOptions args =
  case GetOpt.getOpt GetOpt.Permute hapOptionDescriptions args of
    (options, [], []) ->
      Right (foldl (flip id) defaultHapOptions options)

    (options, [command], []) ->
      case parseHapCommand command of
        Nothing ->
          Left ("Invalid argument: " ++ command)

        maybeHC ->
          Right (foldl (flip id) defaultHapOptions {hapCommand = maybeHC} options)

    _ ->
      Left "First argument must be either 'deploy' or 'rollback'."

hapHelpAction :: Maybe HapCommand -> IO ()
hapHelpAction _ =
  putStrLn hapUsage >> Exit.exitSuccess

hapUsage :: String
hapUsage =
  GetOpt.usageInfo hapUsageHeader hapOptionDescriptions

hapUsageHeader :: String
hapUsageHeader =
  "usage: hap [-h | --help] <command>\n"

main :: IO ()
main = do
  eitherHapOptions <- fmap parseHapOptions Environment.getArgs

  HapOptions{..} <- either Exit.die return eitherHapOptions

  Monad.when hapHelp (hapHelpAction hapCommand)

  hapConfiguration <- configFromEnv

  case hapCommand of
    Just HapDeploy -> deploy hapConfiguration

    Just HapRollback -> rollback hapConfiguration

    Nothing -> hapHelpAction Nothing

splitInColon :: String -> (String, String)
splitInColon = break (== ':')

parseHostWithPort :: Maybe String -> Maybe String
parseHostWithPort Nothing = Nothing
parseHostWithPort (Just server) =
  let
    (host, port) = splitInColon server
  in
    if null port then
      Just host
    else
      Just (unwords [host, "-p", tail port])
