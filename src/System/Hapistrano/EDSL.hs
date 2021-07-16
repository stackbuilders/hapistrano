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

data Bar m = Bar
  { barTasks :: M.HashMap String (m ())
  , barBeforeHooks :: M.HashMap String [String]
  , barAfterHooks :: M.HashMap String [String]
  }

emptyBar :: Bar m
emptyBar = Bar mempty mempty mempty

-- TODO: Rename type and constructors
data Foo m
  = FooTask (Task m)
  | FooHook Hook

data Task m
  = Namespace String [Task m]
  | Task String (m ())

data Hook
  = After String String
  | Before String String

namespace :: String -> Writer [Task m] () -> Writer [Foo m] ()
namespace name tasks = tell [FooTask $ Namespace name $ execWriter tasks]

task :: String -> m () -> Writer [Task m] ()
task name f = tell [Task name f]

invoke :: MonadIO m => IORef (Bar m) -> String -> m ()
invoke ref name = do
  tasks <- barTasks <$> liftIO (readIORef ref)
  case M.lookup name tasks of
    Nothing -> undefined
    Just task -> task


run :: String -> IO ()
run name = do
  ref <- newIORef emptyBar
  let context = Bar
        { barTasks = undefined
        , barBeforeHooks = undefined
        , barAfterHooks = undefined
        }
  writeIORef ref context
  case M.lookup name $ barTasks context of
    Nothing -> undefined
    Just task -> task

type Context m = M.HashMap String (m ())

defaultTasks :: MonadIO m => IORef (Bar m) -> Writer [Foo m] ()
defaultTasks ref = do
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
