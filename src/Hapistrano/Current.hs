{-# LANGUAGE RecordWildCards #-}

module Hapistrano.Current where

import           Development.Shake

import           Hapistrano.Types

linkCurrent :: ReleasePath -> CurrentPath -> Action ()
linkCurrent ReleasePath{..} CurrentPath{..} = do
  cmd "rm -f" unReleasePath :: Action ()
  cmd "ln -s" unReleasePath unCurrentPath
