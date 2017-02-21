{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Numeric.Natural
import Options.Applicative
import Path
import Path.IO
import Paths_hapistrano (version)
import System.Exit
import System.Hapistrano.Types
import qualified Config as C
import qualified Data.Yaml as Yaml
import qualified System.Hapistrano as Hap
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
  , optsConfigFile :: FilePath
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
  <*> strOption
  ( long "config"
  <> short 'c'
  <> value "hap.yaml"
  <> metavar "PATH"
  <> showDefault
  <> help "Configuration file to use" )

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
  <> showDefault
  <> help "How many releases to keep" )

rollbackParser :: Parser Command
rollbackParser = Rollback
  <$> option auto
  ( long "use-nth"
  <> short 'n'
  <> value 1
  <> showDefault
  <> help "How many deployments back to go?" )

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

  econfig <- Yaml.decodeFileEither optsConfigFile
  case econfig of
    Left err -> do
      putStrLn (Yaml.prettyPrintParseException err)
      exitFailure
    Right C.Config {..} ->
      Hap.runHapistrano (SshOptions <$> configHost <*> pure configPort) $ case optsCommand of
        Deploy releaseFormat n -> do
          release <- Hap.pushRelease Task
            { taskDeployPath    = configDeployPath
            , taskRepository    = configRepo
            , taskRevision      = configRevision
            , taskReleaseFormat = releaseFormat }
          rpath <- Hap.releasePath configDeployPath release
          forM_ configCopyFiles $ \(C.CopyThing src dest) -> do
            srcPath  <- resolveFile' src
            destPath <- parseRelFile dest
            Hap.scpFile srcPath (rpath </> destPath)
          forM_ configCopyDirs $ \(C.CopyThing src dest) -> do
            srcPath  <- resolveDir' src
            destPath <- parseRelDir dest
            Hap.scpDir srcPath (rpath </> destPath)
          forM_ configBuildScript (Hap.playScript configDeployPath release)
          Hap.registerReleaseAsComplete configDeployPath release
          Hap.activateRelease configDeployPath release
          Hap.dropOldReleases configDeployPath n
          forM_ configRestartCommand Hap.exec
        Rollback n -> do
          Hap.rollback configDeployPath n
          forM_ configRestartCommand Hap.exec
