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

data Context m = Context
  { contextTasks :: M.HashMap String (m ())
  , contextBeforeHooks :: M.HashMap String [String]
  , contextAfterHooks :: M.HashMap String [String]
  }

emptyContext :: Context m
emptyContext = Context mempty mempty mempty

data Step m
  = StepTask (Task m)
  | StepHook Hook

data Task m
  = Namespace String [Task m]
  | Task String (m ())

data Hook
  = After String String
  | Before String String

namespace :: String -> Writer [Task m] () -> Writer [Step m] ()
namespace name tasks = tell [StepTask $ Namespace name $ execWriter tasks]

task :: String -> m () -> Writer [Task m] ()
task name f = tell [Task name f]

invoke :: MonadIO m => IORef (Context m) -> String -> m ()
invoke ref name = do
  tasks <- contextTasks <$> liftIO (readIORef ref)
  case M.lookup name tasks of
    Nothing -> undefined
    Just task -> task


run :: String -> IO ()
run name = do
  ref <- newIORef emptyContext
  let context = Context
        { contextTasks = undefined
        , contextBeforeHooks = undefined
        , contextAfterHooks = undefined
        }
  writeIORef ref context
  case M.lookup name $ contextTasks context of
    Nothing -> undefined
    Just task -> task

deploySteps :: MonadIO m => IORef (Context m) -> Writer [Step m] ()
deploySteps ref = do
  namespace "deploy" $ do
    task "starting" $ do
      liftIO $ putStrLn "Starting"
      invoke ref "deploy:started"

    task "started" $ do
      liftIO $ putStrLn "Started"
      invoke ref "deploy:updating"

    task "updating" $ do
      liftIO $ putStrLn "Updating"

  {- task "deploy" $ do
    liftIO $ putStrLn "Deploy"
    invoke ref "deploy:starting" -}


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
