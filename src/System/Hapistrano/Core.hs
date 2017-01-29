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
  , exec )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy
import System.Exit
import System.Hapistrano.Commands
import System.Hapistrano.Types
import System.IO
import System.Process

-- | Run the 'Hapistrano' monad. The monad hosts 'exec' actions.

runHapistrano :: MonadIO m
  => Maybe SshOptions  -- ^ SSH options to use or 'Nothing' if we run locally
  -> Hapistrano a      -- ^ The computation to run
  -> m a               -- ^ IO-enabled monad that hosts the computation
runHapistrano sshOptions m = liftIO $ do
  let config = Config
        { configSshOptions = sshOptions }
  r <- runReaderT (runExceptT m) config
  case r of
    Left (Failure n msg) -> do
      forM_ msg (hPutStrLn stderr)
      exitWith (ExitFailure n)
    Right x ->
      x <$ putStrLn "Success."

-- | Fail returning the following status code and printing given message to
-- 'stderr'.

failWith :: Int -> Maybe String -> Hapistrano a
failWith n msg = throwError (Failure n msg)

-- | Run the given sequence of command. Whether to use SSH or not is
-- determined from settings contained in the 'Hapistrano' monad
-- configuration. Commands that return non-zero exit codes will result in
-- short-cutting of execution.

exec :: forall a. Command a => a -> Hapistrano (Result a)
exec typedCmd = do
  Config {..} <- ask
  let (prog, args) =
        case configSshOptions of
          Nothing ->
            ("bash", ["-c", cmd])
          Just SshOptions {..} ->
            ("ssh", [sshHost, "-p", show sshPort, cmd])
      cmd = renderCommand typedCmd
      hostLabel =
        case configSshOptions of
          Nothing              -> "localhost"
          Just SshOptions {..} -> sshHost ++ ":" ++ show sshPort
  liftIO $ do
    printLine hostLabel
    putStrLn ("$ " ++ cmd)
  (exitCode, stdout', stderr') <- liftIO
    (readProcessWithExitCode prog args "")
  unless (null stdout') . liftIO $
    putStrLn stdout'
  unless (null stderr') . liftIO $
    hPutStrLn stderr stderr'
  case exitCode of
    ExitSuccess ->
      return (parseResult (Proxy :: Proxy a) stdout')
    ExitFailure n ->
      failWith n Nothing

----------------------------------------------------------------------------
-- Helpers

-- | Print something “inside” a line, sort-of beautifully.

printLine :: String -> IO ()
printLine str = putStrLn ("*** " ++ str ++ padding)
  where
    padding = ' ' : replicate (75 - length str) '*'
