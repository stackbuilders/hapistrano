{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import qualified Config                     as C
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Monoid                ((<>))
import           Data.Version               (showVersion)
import qualified Data.Yaml.Config           as Yaml
import           Development.GitRev
import           Formatting
import           Numeric.Natural
import           Options.Applicative        hiding (str)
import           Path
import           Path.IO
import           Paths_hapistrano           (version)
import           System.Exit
import qualified System.Hapistrano          as Hap
import qualified System.Hapistrano.Commands as Hap
import qualified System.Hapistrano.Core     as Hap
import           System.Hapistrano.Types
import           System.IO

----------------------------------------------------------------------------
-- Command line options

-- | Command line options.

data Opts = Opts
  { optsCommand    :: Command
  , optsConfigFile :: FilePath
  }

-- | Command to execute and command-specific options.

data Command
  = Deploy (Maybe ReleaseFormat) (Maybe Natural) -- ^ Deploy a new release (with timestamp
    -- format and how many releases to keep)
  | Rollback Natural -- ^ Rollback to Nth previous release

parserInfo :: ParserInfo Opts
parserInfo =
  info
    (helper <*> versionOption <*> optionParser)
    (fullDesc <> progDesc "Deploy tool for Haskell applications" <>
     header "Hapistrano - A deployment library for Haskell applications")
  where
    versionOption :: Parser (a -> a)
    versionOption = infoOption
      (formatToString
        ("Hapistrano: "% string
          % "\nbranch: " % string
          % "\nrevision: " % string)
        (showVersion version)
        $(gitBranch)
        $(gitHash))
      (long "version" <> short 'v' <> help "Show version information")

optionParser :: Parser Opts
optionParser = Opts
  <$> hsubparser
  ( command "deploy"
    (info deployParser (progDesc "Deploy a new release")) <>
    command "rollback"
    (info rollbackParser (progDesc "Roll back to Nth previous release")) )
  <*> strOption
  ( long "config"
  <> short 'c'
  <> value "hap.yaml"
  <> metavar "PATH"
  <> showDefault
  <> help "Configuration file to use" )

deployParser :: Parser Command
deployParser = Deploy
  <$> optional
        ( option pReleaseFormat
            ( long "release-format"
            <> short 'r'
            <> help "Which format release timestamp format to use: ‘long’ or ‘short’, default is ‘short’."
            )
        )
  <*> optional
        ( option auto
            ( long "keep-releases"
            <> short 'k'
            <> help "How many releases to keep, default is '5'"
            )
        )

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

-- | Message that is used for communication between worker threads and the
-- printer thread.

data Message
  = PrintMsg OutputDest String -- ^ Print a message to specified 'OutputDest'
  | FinishMsg                  -- ^ The worker has finished
  deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
  Opts {..} <- execParser parserInfo
  C.Config{..} <- Yaml.loadYamlSettings [optsConfigFile] [] Yaml.useEnv
  chan <- newTChanIO
  let task rf = Task { taskDeployPath    = configDeployPath
                     , taskRepository    = configRepo
                     , taskRevision      = configRevision
                     , taskReleaseFormat = rf }
  let printFnc dest str = atomically $
        writeTChan chan (PrintMsg dest str)
      hap shell sshOpts =  do
        r <- Hap.runHapistrano sshOpts shell printFnc $
          case optsCommand of
            Deploy cliReleaseFormat cliKeepReleases -> do
              let releaseFormat = fromMaybeReleaseFormat cliReleaseFormat configReleaseFormat
                  keepReleases = fromMaybeKeepReleases cliKeepReleases configKeepReleases
              forM_ configRunLocally Hap.playScriptLocally
              release <- if configVcAction
                          then Hap.pushRelease (task releaseFormat)
                          else Hap.pushReleaseWithoutVc (task releaseFormat)
              rpath <- Hap.releasePath configDeployPath release
              forM_ configCopyFiles $ \(C.CopyThing src dest) -> do
                srcPath  <- resolveFile' src
                destPath <- parseRelFile dest
                let dpath = rpath </> destPath
                (Hap.exec . Hap.MkDir . parent) dpath
                Hap.scpFile srcPath dpath
              forM_ configCopyDirs $ \(C.CopyThing src dest) -> do
                srcPath  <- resolveDir' src
                destPath <- parseRelDir dest
                let dpath = rpath </> destPath
                (Hap.exec . Hap.MkDir . parent) dpath
                Hap.scpDir srcPath dpath
              forM_ configLinkedFiles
                (Hap.linkToShared configTargetSystem rpath configDeployPath)
              forM_ configLinkedDirs
                (Hap.linkToShared configTargetSystem rpath configDeployPath)
              forM_ configBuildScript (Hap.playScript configDeployPath release)
              Hap.registerReleaseAsComplete configDeployPath release
              Hap.activateRelease configTargetSystem configDeployPath release
              Hap.dropOldReleases configDeployPath keepReleases
              forM_ configRestartCommand Hap.exec
            Rollback n -> do
              Hap.rollback configTargetSystem configDeployPath n
              forM_ configRestartCommand Hap.exec
        atomically (writeTChan chan FinishMsg)
        return r
      printer :: Int -> IO ()
      printer n = when (n > 0) $ do
        msg <- atomically (readTChan chan)
        case msg of
          PrintMsg StdoutDest str ->
            putStr str >> printer n
          PrintMsg StderrDest str ->
            hPutStr stderr str >> printer n
          FinishMsg ->
            printer (n - 1)
      haps :: [IO (Either Int ())]
      haps =
        case configHosts of
          [] -> [hap Bash Nothing] -- localhost, no SSH
          xs ->
            let runHap (C.Target{..}) =
                  hap targetShell (Just $ SshOptions targetHost targetPort targetSshArgs)
            in runHap <$> xs
  results <- (runConcurrently . traverse Concurrently)
    ((Right () <$ printer (length haps)) : haps)
  case sequence_ results of
    Left n   -> exitWith (ExitFailure n)
    Right () -> putStrLn "Success."
