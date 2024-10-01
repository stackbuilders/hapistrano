{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Data.Monoid                   ((<>))
#endif
import           Data.Version                  (showVersion)
import qualified Data.Yaml.Config              as Yaml
import           Development.GitRev
import           Formatting                    (formatToString, string, (%))
import           Options.Applicative           hiding (str)
import           Paths_hapistrano              (version)
import           System.Exit
import qualified System.Hapistrano             as Hap
import qualified System.Hapistrano.Config      as C
import qualified System.Hapistrano.Maintenance as Hap
import           System.Hapistrano.Types
import           System.IO

----------------------------------------------------------------------------

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
    (info rollbackParser (progDesc "Roll back to Nth previous release")) <>
    command "maintenance"
    (info maintenanceParser (progDesc "Enable/Disable maintenance mode")) <>
    command "init"
    (info initParser (progDesc "Initialize hapistrano file"))
    )
  <*> strOption
  ( long "config"
  <> short 'c'
  <> value "hap.yaml"
  <> metavar "PATH"
  <> showDefault
  <> help "Configuration file to use" )
  <*> flag
  False
  True
  ( long "dry-run"
  <> help "Display command (without running them)" )

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
  <*> switch
        ( long "keep-one-failed"
            <> help "Keep all failed releases or just one -the latest-, default (without using this flag) is to keep all failed releases."
        )

rollbackParser :: Parser Command
rollbackParser = Rollback
  <$> option auto
  ( long "use-nth"
  <> short 'n'
  <> value 1
  <> showDefault
  <> help "How many deployments back to go?" )

initParser :: Parser Command
initParser = pure InitConfig

maintenanceParser :: Parser Command
maintenanceParser =
  Maintenance
    <$> hsubparser
      ( command "enable" (info (pure Enable) (progDesc "Enables maintenance mode"))
          <> command "disable" (info (pure Disable) (progDesc "Disables maintenance mode"))
      )

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
  opts@Opts{..} <- execParser parserInfo
  case optsCommand of
    Deploy cliReleaseFormat cliKeepReleases cliKeepOneFailed ->
      runHapCmd opts $ \hapConfig@C.Config{..} executionMode ->
        Hap.deploy
          hapConfig
          (fromMaybeReleaseFormat cliReleaseFormat configReleaseFormat)
          (fromMaybeKeepReleases cliKeepReleases configKeepReleases)
          (cliKeepOneFailed || configKeepOneFailed)
          executionMode
    Rollback n ->
      runHapCmd opts $ \C.Config{..} _ ->
        Hap.rollback configTargetSystem configDeployPath n configRestartCommand
    Maintenance Enable ->
      runHapCmd opts $ \C.Config{..} _ ->
        Hap.writeMaintenanceFile configDeployPath configMaintenanceDirectory configMaintenanceFileName
    Maintenance _ ->
      runHapCmd opts $ \C.Config{..} _ ->
        Hap.deleteMaintenanceFile configDeployPath configMaintenanceDirectory configMaintenanceFileName
    InitConfig -> Hap.initConfig getLine

runHapCmd :: Opts -> (C.Config -> C.ExecutionMode -> Hapistrano ()) -> IO ()
runHapCmd Opts{..} hapCmd = do
  hapConfig@C.Config{..} <- Yaml.loadYamlSettings [optsConfigFile] [] Yaml.useEnv
  chan <- newTChanIO
  let printFnc dest str = atomically $
        writeTChan chan (PrintMsg dest str)
      hap shell sshOpts executionMode = do
        r <- Hap.runHapistrano optsDryRun sshOpts shell printFnc $ hapCmd hapConfig executionMode
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
          [] -> [hap Bash Nothing C.LeadTarget] -- localhost, no SSH
          targets@(leadTarget : _) ->
            let runHap currentTarget@C.Target{..} =
                  hap targetShell (Just $ SshOptions targetHost targetPort targetSshArgs)
                    (if leadTarget == currentTarget then C.LeadTarget else C.AllTargets)
            in runHap <$> targets

  results <- (runConcurrently . traverse Concurrently)
    ((Right () <$ printer (length haps)) : haps)
  case sequence_ results of
    Left n   -> exitWith (ExitFailure n)
    Right () -> putStrLn "Success."
