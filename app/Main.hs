{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE CPP               #-}

module Main (main) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Data.Monoid                ((<>))
#endif
import           Data.Version               (showVersion)
import qualified Data.Yaml.Config           as Yaml
import           Development.GitRev
import           Formatting                 (formatToString, string, (%))
import           Options.Applicative        hiding (str)
import           Path
import           Path.IO
import           Paths_hapistrano           (version)
import           System.Exit
import qualified System.Hapistrano          as Hap
import qualified System.Hapistrano.Commands as Hap
import qualified System.Hapistrano.Config   as C
import qualified System.Hapistrano.Core     as Hap
import           System.Hapistrano.Types
import           System.IO
import System.Hapistrano (createHapistranoDeployState)
import Control.Monad.Error.Class (throwError, catchError)

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
  <*> switch
            ( long "keep-one-failed"
            <> short 'f'
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
  Opts{..} <- execParser parserInfo
  C.Config{..} <- Yaml.loadYamlSettings [optsConfigFile] [] Yaml.useEnv
  chan <- newTChanIO
  let task rf = Task { taskDeployPath    = configDeployPath
                     , taskSource        = configSource
                     , taskReleaseFormat = rf }
  let failStateAndThrow e@(_, maybeRelease) =
        case maybeRelease of
          (Just release) -> createHapistranoDeployState configDeployPath release Fail >> throwError e
          Nothing -> throwError e
  let printFnc dest str = atomically $
        writeTChan chan (PrintMsg dest str)
      hap shell sshOpts = do
        r <- Hap.runHapistrano sshOpts shell printFnc C.Config{..} $
          case optsCommand of
            Deploy cliReleaseFormat cliKeepReleases cliKeepOneFailed -> do
              let releaseFormat = fromMaybeReleaseFormat cliReleaseFormat configReleaseFormat
                  keepReleases = fromMaybeKeepReleases cliKeepReleases configKeepReleases
                  keepOneFailed = cliKeepOneFailed || configKeepOneFailed
              forM_ configRunLocally Hap.playScriptLocally
              release <- if configVcAction
                          then Hap.pushRelease (task releaseFormat)
                          else Hap.pushReleaseWithoutVc (task releaseFormat)
              rpath <- Hap.releasePath configDeployPath release configWorkingDir
              forM_ (toMaybePath configSource) $ \src ->
                Hap.scpDir src rpath (Just release)
              forM_ configCopyFiles $ \(C.CopyThing src dest) -> do
                srcPath  <- resolveFile' src
                destPath <- parseRelFile dest
                let dpath = rpath </> destPath
                (flip Hap.exec (Just release) . Hap.MkDir . parent) dpath
                Hap.scpFile srcPath dpath (Just release)
              forM_ configCopyDirs $ \(C.CopyThing src dest) -> do
                srcPath  <- resolveDir' src
                destPath <- parseRelDir dest
                let dpath = rpath </> destPath
                (flip Hap.exec (Just release) . Hap.MkDir . parent) dpath
                Hap.scpDir srcPath dpath (Just release)
              forM_ configLinkedFiles
                $ flip (Hap.linkToShared configTargetSystem rpath configDeployPath) (Just release)
              forM_ configLinkedDirs
                $ flip (Hap.linkToShared configTargetSystem rpath configDeployPath) (Just release)
              forM_ configBuildScript (Hap.playScript configDeployPath release configWorkingDir)
              Hap.registerReleaseAsComplete configDeployPath release
              Hap.activateRelease configTargetSystem configDeployPath release
              Hap.dropOldReleases configDeployPath keepReleases
              forM_ configRestartCommand (flip Hap.exec $ Just release)
              Hap.createHapistranoDeployState configDeployPath release System.Hapistrano.Types.Success
            Rollback n -> do
              Hap.rollback configTargetSystem configDeployPath n
              forM_ configRestartCommand (flip Hap.exec Nothing)
          `catchError` failStateAndThrow
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
            let runHap C.Target{..} =
                  hap targetShell (Just $ SshOptions targetHost targetPort targetSshArgs)
            in runHap <$> xs
  results <- (runConcurrently . traverse Concurrently)
    ((Right () <$ printer (length haps)) : haps)
  case sequence_ results of
    Left n   -> exitWith (ExitFailure n)
    Right () -> putStrLn "Success."
