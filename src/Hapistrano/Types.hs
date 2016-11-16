{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hapistrano.Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default
import           Data.Text
import           Network.URL

data Config = Config
  { configBranch       :: Branch
  , configKeepReleases :: KeepReleases
  , configRepoUrl      :: RepoUrl
  , configDeployPath   :: FilePath
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config <$> o .:? "branch" .!= def
           <*> o .:? "keepReleases" .!= def
           <*> o .: "repoUrl"
           <*> o .: "deployPath"

newtype Branch = Branch { unBranch :: String }
  deriving (Eq, Show, FromJSON)

instance Default Branch where
  def = Branch "master"

newtype KeepReleases = KeepReleases { unKeepReleases :: Int }
  deriving (Eq, Show, FromJSON)

instance Default KeepReleases where
  def = KeepReleases 5

newtype RepoUrl = RepoUrl { unRepoUrl :: URL }
  deriving (Eq, Show)

instance FromJSON RepoUrl where
  parseJSON = withText "RepoUrl" $ \t -> do
    case importURL (unpack t) of
      Nothing  -> typeMismatch "RepoUrl" (String t)
      Just url -> return $ RepoUrl url
