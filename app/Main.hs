{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Numeric.Natural
import Options.Applicative
import Path
import Path.IO
import Paths_hapistrano (version)
import System.Environment.Compat (lookupEnv, getEnv)
import System.Exit
import System.Hapistrano.Types
import Text.Read (readMaybe)
import qualified System.Hapistrano as Hap
import qualified System.Hapistrano.Commands as Hap
import qualified System.Hapistrano.Core as Hap

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

----------------------------------------------------------------------------
-- Command line options

-- | Command line options.

data Opts = Opts
  { optsCommand :: Command
  , optsVersion :: Bool
  }

-- | Command to execute and command-specific options.

data Command
  = Deploy ReleaseFormat Natural -- ^ Deploy a new release (with timestamp
    -- format and how many releases to keep)
  | Rollback Natural -- ^ Rollback to Nth previous release

parserInfo :: ParserInfo Opts
parserInfo = info (helper <*> optionParser)
  ( fullDesc <>
    progDesc "Deploy tool for Haskell applications" <>
    header "Hapistrano - A deployment library for Haskell applications" )

optionParser :: Parser Opts
optionParser = Opts
  <$> subparser
  ( command "deploy"
    (info deployParser (progDesc "Deploy a new release")) <>
    command "rollback"
    (info rollbackParser (progDesc "Roll back to Nth previous release")) )
  <*> switch
  ( long "version"
  <> short 'v'
  <> help "Show version of the program" )

deployParser :: Parser Command
deployParser = Deploy
  <$> option pReleaseFormat
  ( long "release-format"
  <> short 'r'
  <> value ReleaseShort
  <> help "Which format release timestamp format to use: ‘long’ or ‘short’, default is ‘short’." )
  <*> option auto
  ( long "keep-releases"
  <> short 'k'
  <> value 5
  <> help "How many releases to keep. Default is 5." )

rollbackParser :: Parser Command
rollbackParser = Rollback
  <$> option auto
  ( long "use-nth"
  <> short 'n'
  <> value 1
  <> help "How many deployments back to go? Default is 1." )

pReleaseFormat :: ReadM ReleaseFormat
pReleaseFormat = eitherReader $ \s ->
  case s of
    "long"  -> Right ReleaseLong
    "short" -> Right ReleaseShort
    _       -> Left ("Unknown format: " ++ s ++ ", try ‘long’ or ‘short’.")

----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  Opts {..} <- execParser parserInfo
  when optsVersion $ do
    putStrLn $ "Hapistrano " ++ showVersion version
    exitSuccess

  deployPath  <- getEnv "DEPLOY_PATH" >>= parseAbsDir
  repository  <- getEnv "REPOSITORY"
  revision    <- getEnv "REVISION"
  port        <- fromMaybe 22 . (>>= readMaybe) <$> lookupEnv "PORT"
  mhost       <- lookupEnv "HOST"
  buildScript <- lookupEnv "BUILD_SCRIPT"
  mrestartCmd <- (>>= Hap.mkGenericCommand) <$> lookupEnv "RESTART_COMMAND"

  Hap.runHapistrano (SshOptions <$> mhost <*> pure port) $ case optsCommand of
    Deploy releaseFormat n -> do
      release <- Hap.pushRelease Task
        { taskDeployPath    = deployPath
        , taskRepository    = repository
        , taskRevision      = revision
        , taskReleaseFormat = releaseFormat }
      forM_ buildScript $ \spath' -> do
        spath  <- resolveFile' spath'
        script <- Hap.readScript spath
        Hap.playScript script deployPath release
      Hap.activateRelease deployPath release
      Hap.dropOldReleases deployPath n
    Rollback n -> do
      Hap.rollback deployPath n
      forM_ mrestartCmd Hap.exec
