{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hapistrano.Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default
import           Data.Text
import           Development.Shake
import           Network.URL

data Config = Config
  { configBranch       :: Branch
  , configDeployPath   :: FilePath
  , configKeepReleases :: KeepReleases
  , configLinkedFiles  :: [FilePath]
  , configLogLevel     :: LogLevel
  , configRepoUrl      :: RepoUrl
  , configScriptPath   :: FilePath
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config <$> o .:? "branch" .!= def
           <*> o .: "deployPath"
           <*> o .:? "keepReleases" .!= def
           <*> o .: "linkedFiles"
           <*> o .:? "logLevel" .!= def
           <*> o .: "repoUrl"
           <*> o .: "scriptPath"

newtype Branch = Branch { unBranch :: String }
  deriving (Eq, Show, FromJSON)

instance Default Branch where
  def = Branch "master"

newtype KeepReleases = KeepReleases { unKeepReleases :: Int }
  deriving (Eq, Show, FromJSON)

instance Default KeepReleases where
  def = KeepReleases 5

newtype LogLevel = LogLevel { unLogLevel :: Verbosity }
  deriving (Eq, Show)

instance Default LogLevel where
  def = LogLevel Normal

instance FromJSON LogLevel where
  parseJSON (String "silent") = return $ LogLevel Silent
  parseJSON (String "quiet") = return $ LogLevel Quiet
  parseJSON (String "normal") = return $ LogLevel Normal
  parseJSON (String "loud") = return $ LogLevel Loud
  parseJSON (String "chatty") = return $ LogLevel Chatty
  parseJSON (String "diagnostic") = return $ LogLevel Diagnostic
  parseJSON v = typeMismatch "LogLevel" v

newtype RepoUrl = RepoUrl { unRepoUrl :: URL }
  deriving (Eq, Show)

instance FromJSON RepoUrl where
  parseJSON = withText "RepoUrl" $ \t -> do
    case importURL (unpack t) of
      Nothing  -> typeMismatch "RepoUrl" (String t)
      Just url -> return $ RepoUrl url
