{-# LANGUAGE RecordWildCards #-}

module System.Hapistrano.Current
  ( linkCurrent
  ) where

import           Development.Shake

import           System.Hapistrano.NewTypes

linkCurrent :: ReleasePath -> CurrentPath -> Action ()
linkCurrent ReleasePath{..} CurrentPath{..} = do
  cmd "rm -f" unReleasePath :: Action ()
  cmd "ln -s" unReleasePath unCurrentPath
