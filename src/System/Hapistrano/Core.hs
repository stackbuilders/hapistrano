-- |
-- Module      :  System.Hapistrano.Core
-- Copyright   :  © 2015-2017 Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Core Hapistrano functions that provide basis on which all the
-- functionality is built.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Hapistrano.Core
  ( runHapistrano
  , failWith
  , exec
  , execWithInheritStdout
  , scpFile
  , scpDir )
where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Concurrent.STM (atomically)
import           Data.Proxy
import           Path
import           System.Exit
import           System.Hapistrano.Commands
import           System.Hapistrano.Types
import           System.Process
import           System.Process.Typed (ProcessConfig)
import qualified System.Process.Typed as SPT

-- | Run the 'Hapistrano' monad. The monad hosts 'exec' actions.

runHapistrano :: MonadIO m
  => Maybe SshOptions  -- ^ SSH options to use or 'Nothing' if we run locally
  -> Shell             -- ^ Shell to run commands
  -> (OutputDest -> String -> IO ()) -- ^ How to print messages
  -> Hapistrano a      -- ^ The computation to run
  -> m (Either Int a)  -- ^ Status code in 'Left' on failure, result in
              -- 'Right' on success
runHapistrano sshOptions shell' printFnc m = liftIO $ do
  let config = Config
        { configSshOptions = sshOptions
        , configShellOptions = shell'
        , configPrint      = printFnc }
  r <- runReaderT (runExceptT m) config
  case r of
    Left (Failure n msg) -> do
      forM_ msg (printFnc StderrDest)
      return (Left n)
    Right x -> return (Right x)

-- | Fail returning the following status code and message.

failWith :: Int -> Maybe String -> Hapistrano a
failWith n msg = throwError (Failure n msg)

-- | Run the given sequence of command. Whether to use SSH or not is
-- determined from settings contained in the 'Hapistrano' monad
-- configuration. Commands that return non-zero exit codes will result in
-- short-cutting of execution.
-- __NOTE:__ the commands executed with 'exec' will create their own pipe and
-- will stream output there and once the command finishes its execution it will
-- parse the result.

exec :: forall a. Command a => a -> Hapistrano (Result a)
exec typedCmd = do
  let cmd = renderCommand typedCmd
  (prog, args) <- getProgAndArgs cmd
  parseResult (Proxy :: Proxy a) <$> exec' cmd (readProcessWithExitCode prog args "")

-- | Same as 'exec' but it streams to stdout only for _GenericCommand_s

execWithInheritStdout :: Command a => a -> Hapistrano ()
execWithInheritStdout typedCmd = do
  let cmd = renderCommand typedCmd
  (prog, args) <- getProgAndArgs cmd
  void $ exec' cmd (readProcessWithExitCode' (SPT.proc prog args))
    where
    -- | Prepares a process, reads @stdout@ and @stderr@ and returns exit code
    -- NOTE: @strdout@ and @stderr@ are empty string because we're writing
    -- the output to the parent.
    readProcessWithExitCode'
      :: ProcessConfig stdin stdoutIgnored stderrIgnored
      -> IO (ExitCode, String, String)
    readProcessWithExitCode' pc =
      SPT.withProcess pc' $ \p -> atomically $
        (,,) <$> SPT.waitExitCodeSTM p
             <*> return ""
             <*> return ""
        where
          pc' = SPT.setStdout SPT.inherit
              $ SPT.setStderr SPT.inherit pc

-- | Get program and args to run a command locally or remotelly.

getProgAndArgs :: String -> Hapistrano (String, [String])
getProgAndArgs cmd = do
  Config {..} <- ask
  return $
    case configSshOptions of
      Nothing ->
        (renderShell configShellOptions, ["-c", cmd])
      Just SshOptions {..} ->
        ("ssh", [sshHost, "-p", show sshPort, cmd])
    where
      renderShell :: Shell -> String
      renderShell Zsh = "zsh"
      renderShell Bash = "bash"

-- | Copy a file from local path to target server.

scpFile
  :: Path Abs File     -- ^ Location of the file to copy
  -> Path Abs File     -- ^ Where to put the file on target machine
  -> Hapistrano ()
scpFile src dest =
  scp' (fromAbsFile src) (fromAbsFile dest) ["-q"]

-- | Copy a local directory recursively to target server.

scpDir
  :: Path Abs Dir      -- ^ Location of the directory to copy
  -> Path Abs Dir      -- ^ Where to put the dir on target machine
  -> Hapistrano ()
scpDir src dest =
  scp' (fromAbsDir src) (fromAbsDir dest) ["-qr"]

scp'
  :: FilePath
  -> FilePath
  -> [String]
  -> Hapistrano ()
scp' src dest extraArgs = do
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
  void (exec' (prog ++ " " ++ unwords args) (readProcessWithExitCode prog args ""))

----------------------------------------------------------------------------
-- Helpers

-- | A helper for 'exec' and similar functions.

exec'
  :: String            -- ^ How to show the command in print-outs
  -> IO (ExitCode, String, String) -- ^ Handler to get (ExitCode, Output, Error) it can change accordingly to @stdout@ and @stderr@ of child process
  -> Hapistrano String -- ^ Raw stdout output of that program
exec' cmd readProcessOutput = do
  Config {..} <- ask
  let hostLabel =
        case configSshOptions of
          Nothing              -> "localhost"
          Just SshOptions {..} -> sshHost ++ ":" ++ show sshPort
  liftIO $ configPrint StdoutDest (putLine hostLabel ++ "$ " ++ cmd ++ "\n")
  (exitCode', stdout', stderr') <- liftIO readProcessOutput
  unless (null stdout') . liftIO $
    configPrint StdoutDest stdout'
  unless (null stderr') . liftIO $
    configPrint StderrDest stderr'
  case exitCode' of
    ExitSuccess ->
      return stdout'
    ExitFailure n ->
      failWith n Nothing

-- | Put something “inside” a line, sort-of beautifully.

putLine :: String -> String
putLine str = "*** " ++ str ++ padding ++ "\n"
  where
    padding = ' ' : replicate (75 - length str) '*'
