-- |
-- Module      :  System.Hapistrano.Core
-- Copyright   :  © 2015-Present Stack Builders
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- Core Hapistrano functions that provide basis on which all the
-- functionality is built.
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Hapistrano.Core
  ( failWith
  , exec
  , execWithInheritStdout
  , scpFile
  , scpDir )
where

import           Control.Concurrent.STM     (atomically)
import           Control.Monad
import           Control.Monad.Catch        (throwM)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Proxy
import           Data.Time
import           Path
import           System.Console.ANSI
import           System.Exit
import           System.Hapistrano.Commands
import           System.Hapistrano.Types    hiding (Command)
import           System.Process
import           System.Process.Typed       (ProcessConfig)
import qualified System.Process.Typed       as SPT

-- | Fail returning the following status code and message.
failWith :: Int -> Maybe String -> Maybe Release -> Hapistrano a
failWith n msg maybeRelease = throwM $ HapistranoException (Failure n msg, maybeRelease)

-- | Run the given sequence of command. Whether to use SSH or not is
-- determined from settings contained in the 'Hapistrano' monad
-- configuration. Commands that return non-zero exit codes will result in
-- short-cutting of execution.
-- __NOTE:__ the commands executed with 'exec' will create their own pipe and
-- will stream output there and once the command finishes its execution it will
-- parse the result.
exec ::
     forall a. Command a
  => a -- ^ Command being executed
  -> Maybe Release -- ^ Release that was being attempted, if it was defined
  -> Hapistrano (Result a)
exec typedCmd maybeRelease = do
  let cmd = renderCommand typedCmd
  (prog, args) <- getProgAndArgs cmd
  parseResult (Proxy :: Proxy a) <$>
    exec' cmd (readProcessWithExitCode prog args "") maybeRelease

-- | Same as 'exec' but it streams to stdout only for _GenericCommand_s
execWithInheritStdout ::
     Command a
  => a -- ^ Command being executed
  -> Maybe Release -- ^ Release that was being attempted, if it was defined
  -> Hapistrano ()
execWithInheritStdout typedCmd maybeRelease = do
  let cmd = renderCommand typedCmd
  (prog, args) <- getProgAndArgs cmd
  void $ exec' cmd (readProcessWithExitCode' (SPT.proc prog args)) maybeRelease
    where
    -- | Prepares a process, reads @stdout@ and @stderr@ and returns exit code
    -- NOTE: @strdout@ and @stderr@ are empty string because we're writing
    -- the output to the parent.
    readProcessWithExitCode' ::
         ProcessConfig stdin stdoutIgnored stderrIgnored
      -> IO (ExitCode, String, String)
    readProcessWithExitCode' pc =
      SPT.withProcessTerm pc' $ \p ->
        atomically $ (,,) <$> SPT.waitExitCodeSTM p <*> return "" <*> return ""
      where
        pc' = SPT.setStdout SPT.inherit $ SPT.setStderr SPT.inherit pc

-- | Get program and args to run a command locally or remotely.
getProgAndArgs :: String -> Hapistrano (String, [String])
getProgAndArgs cmd = do
  Config {..} <- ask
  return $
    case configSshOptions of
      Nothing -> (renderShell configShellOptions, ["-c", cmd])
      Just SshOptions {..} ->
        ("ssh", sshArgs ++ [sshHost, "-p", show sshPort, cmd])
    where
      renderShell :: Shell -> String
      renderShell Zsh  = "zsh"
      renderShell Bash = "bash"


-- | Copy a file from local path to target server.
scpFile ::
     Path Abs File -- ^ Location of the file to copy
  -> Path Abs File -- ^ Where to put the file on target machine
  -> Maybe Release -- ^ Release that was being attempted, if it was defined
  -> Hapistrano ()
scpFile src dest = scp' (fromAbsFile src) (fromAbsFile dest) ["-q"]

-- | Copy a local directory recursively to target server.
scpDir ::
     Path Abs Dir -- ^ Location of the directory to copy
  -> Path Abs Dir -- ^ Where to put the dir on target machine
  -> Maybe Release -- ^ Release that was being attempted, if it was defined
  -> Hapistrano ()
scpDir src dest = scp' (fromAbsDir src) (fromAbsDir dest) ["-qr"]

scp' :: FilePath -> FilePath -> [String] -> Maybe Release -> Hapistrano ()
scp' src dest extraArgs maybeRelease = do
  Config {..} <- ask
  let prog = "scp"
      portArg =
        case sshPort <$> configSshOptions of
          Nothing -> []
          Just x  -> ["-P", show x]
      hostPrefix =
        case sshHost <$> configSshOptions of
          Nothing -> ""
          Just x  -> x ++ ":"
      args = extraArgs ++ portArg ++ [src, hostPrefix ++ dest]
  void
    (exec' (prog ++ " " ++ unwords args) (readProcessWithExitCode prog args "") maybeRelease)

----------------------------------------------------------------------------
-- Helpers
-- | A helper for 'exec' and similar functions.
exec' ::
     String -- ^ How to show the command in print-outs
  -> IO (ExitCode, String, String) -- ^ Handler to get (ExitCode, Output, Error) it can change accordingly to @stdout@ and @stderr@ of child process
  -> Maybe Release -- ^ Release that was being attempted, if it was defined
  -> Hapistrano String -- ^ Raw stdout output of that program
exec' cmd readProcessOutput maybeRelease = do
  Config {..} <- ask
  time <- liftIO getZonedTime
  let timeStampFormat = "%T,  %F (%Z)"
      printableTime = formatTime defaultTimeLocale timeStampFormat time
      hostLabel =
        case configSshOptions of
          Nothing              -> "localhost"
          Just SshOptions {..} -> sshHost ++ ":" ++ show sshPort
      hostInfo = colorizeString Blue $ putLine hostLabel
      timestampInfo = colorizeString Cyan ("[" ++ printableTime ++ "] INFO -- : $ ")
      cmdInfo = colorizeString Green (cmd ++ "\n")
  liftIO $ configPrint StdoutDest (hostInfo ++ timestampInfo ++ cmdInfo)
  (exitCode', stdout', stderr') <- liftIO readProcessOutput
  unless (null stdout') . liftIO $ configPrint StdoutDest stdout'
  unless (null stderr') . liftIO $ configPrint StderrDest stderr'
  case exitCode' of
    ExitSuccess   -> return stdout'
    ExitFailure n -> failWith n Nothing maybeRelease

-- | Put something “inside” a line, sort-of beautifully.
putLine :: String -> String
putLine str = "*** " ++ str ++ padding ++ "\n"
  where
    padding = ' ' : replicate (75 - length str) '*'

colorizeString :: Color -> String -> String
colorizeString color msg =
  setSGRCode [SetColor Foreground Vivid color] ++ msg ++ setSGRCode [Reset]
