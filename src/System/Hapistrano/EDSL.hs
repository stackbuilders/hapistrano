{-# LANGUAGE RecordWildCards #-}

module System.Hapistrano.EDSL where

import Control.Monad.Writer

data Config = Config
  { configRepoUrl :: String
  , configRepoPath :: String
  }

data Task m
  = Namespace String [Task m]
  | Task String [String] (m ())

namespace :: String -> Writer [Task m] () -> Writer [Task m] ()
namespace name tasks = tell [Namespace name $ execWriter tasks]

task :: String -> [String] -> m () -> Writer [Task m] ()
task name deps f = tell [Task name deps f]

main :: Monad m => Writer [Task m] ()
main =
  namespace "deploy" $ do
    task "check" [] $ do
      undefined

    task "updating" [] $ do
      Config{..} <- getConfig
      execute "git" ["clone", "--mirror", configRepoUrl, configRepoPath]

    task "update" [] $ do
      -- within repoPath
      execute "git" ["remote", "update", "--prune"]

    task "create_release" ["git:update"] $ do
      undefined

getConfig :: m Config
getConfig = undefined

execute :: String -> [String] -> m ()
execute = undefined
