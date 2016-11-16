{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hapistrano.Types where

import           Data.Default
import           Network.URL

data Config = Config
  { configDeployPath   :: FilePath
  , configRepoUrl      :: URL
  , configKeepReleases :: KeepReleases
  } deriving (Eq, Show)

newtype KeepReleases = KeepReleases { unKeepReleases :: Int }
  deriving (Eq, Show)

instance Default KeepReleases where
  def = KeepReleases 5
