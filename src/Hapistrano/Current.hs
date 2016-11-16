{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Current where

import           Development.Shake

linkCurrent :: FilePath -> FilePath -> Action ()
linkCurrent releasePath currentPath = do
  cmd "rm -rf" currentPath :: Action ()
  cmd "ln -s" releasePath currentPath
