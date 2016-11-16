{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Current where

import           Development.Shake

linkCurrent :: FilePath -> FilePath -> Action ()
linkCurrent releasePath currentPath = do
  cmd "rm -f" currentPath :: Action ()
  cmd "ln -s" releasePath currentPath
