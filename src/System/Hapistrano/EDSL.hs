{-# LANGUAGE RecordWildCards #-}

-- TODO: replace all String with more efficient types
module System.Hapistrano.EDSL where

import qualified Data.HashMap.Strict as M

import Control.Monad.Writer
-- import System.Process

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

invoke = undefined

tasks :: Writer [Task IO] ()
tasks =
  namespace "deploy" $ do
    task "starting" $ do
      putStrLn "Starting"
      invoke "deploy:started"

    task "started" $ do
      putStrLn "Started"
      invoke "deploy:updating"

    task "updating" $ do
      putStrLn "Updating"

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
