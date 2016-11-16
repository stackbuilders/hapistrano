{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hapistrano.Types where

import           Data.Default
import           Network.URL

data Config = Config
  { configBranch       :: Branch
  , configKeepReleases :: KeepReleases
  , configRepoUrl      :: URL
  , configDeployPath   :: FilePath
  } deriving (Eq, Show)

newtype Branch = Branch { unBranch :: String }
  deriving (Eq, Show)

instance Default Branch where
  def = Branch "master"

newtype KeepReleases = KeepReleases { unKeepReleases :: Int }
  deriving (Eq, Show)

instance Default KeepReleases where
  def = KeepReleases 5
