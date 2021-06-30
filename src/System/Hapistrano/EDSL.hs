{-# LANGUAGE RecordWildCards #-}

-- TODO: replace all String with more efficient types
module System.Hapistrano.EDSL where

import qualified Data.HashMap.Strict as M

import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef

data Config = Config
  { configDeployTo :: String
  , configRepoUrl :: String
  , configBranch :: String
  }

data Task m
  = Namespace String [Task m]
  | Task String (m ())

namespace :: String -> Writer [Task m] () -> Writer [Task m] ()
namespace name tasks = tell [Namespace name $ execWriter tasks]

task :: String -> m () -> Writer [Task m] ()
task name f = tell [Task name f]

invoke :: MonadIO m => IORef (Context m) -> String -> m ()
invoke ref name = do
  tasks <- liftIO $ readIORef ref
  case M.lookup name tasks of
    Nothing -> undefined
    Just task -> task


run :: String -> IO ()
run name = do
  ref <- newIORef M.empty
  let context = events $ execWriter $ tasks ref
  writeIORef ref context
  case M.lookup name context of
    Nothing -> undefined
    Just task -> task

type Context m = M.HashMap String (m ())

tasks :: MonadIO m => IORef (Context m) -> Writer [Task m] ()
tasks ref =
  namespace "deploy" $ do
    task "starting" $ do
      liftIO $ putStrLn "Starting"
      invoke ref "deploy:started"

    task "started" $ do
      liftIO $ putStrLn "Started"
      invoke ref "deploy:updating"

    task "updating" $ do
      liftIO $ putStrLn "Updating"

events :: [Task m] -> M.HashMap String (m ())
events = M.fromList . concatMap (event Nothing)

event :: Maybe String -> Task m -> [(String, m ())]
event mprefix task =
  case mprefix of
    Nothing ->
      case task of
        Namespace name tasks -> concatMap (event (Just name)) tasks
        Task name f -> [(name, f)]
    Just prefix ->
      case task of
        Namespace name tasks -> concatMap (event (Just (prefix <> ":" <> name))) tasks
        Task name f -> [(prefix <> ":" <> name, f)]
