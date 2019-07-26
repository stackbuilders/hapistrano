-- |
-- Module      :  System.Hapistrano.Types
-- Copyright   :  © 2015-Present Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Juan Paucar <jpaucar@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Type definitions for the Hapistrano tool.

{-# LANGUAGE OverloadedStrings #-}

module System.Hapistrano.Types
  ( Hapistrano
  , Failure (..)
  , Config (..)
  , Task (..)
  , ReleaseFormat(..)
  , SshOptions (..)
  , OutputDest (..)
  , Release
  , TargetSystem(..)
  , Shell(..)
  , mkRelease
  , releaseTime
  , renderRelease
  , parseRelease
  , fromMaybeReleaseFormat
  , fromMaybeKeepReleases )
where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Maybe
import           Data.Time
import           Numeric.Natural
import           Path

-- | Hapistrano monad.

type Hapistrano a = ExceptT Failure (ReaderT Config IO) a

-- | Failure with status code and a message.

data Failure = Failure Int (Maybe String)

-- | Hapistrano configuration options.

data Config = Config
  { configSshOptions :: !(Maybe SshOptions)
    -- ^ 'Nothing' if we are running locally, or SSH options to use.
  , configShellOptions :: !Shell
    -- ^ One of the supported 'Shell's
  , configPrint      :: !(OutputDest -> String -> IO ())
    -- ^ How to print messages
  }

-- | The records describes deployment task.

data Task = Task
  { taskDeployPath    :: Path Abs Dir
    -- ^ The root of the deploy target on the remote host
  , taskRepository    :: String
    -- ^ The URL of remote Git repo to deploy
  , taskRevision      :: String
    -- ^ A SHA1 or branch to release
  , taskReleaseFormat :: ReleaseFormat
    -- ^ The 'ReleaseFormat' to use
  } deriving (Show, Eq, Ord)

-- | Release format mode.

data ReleaseFormat
  = ReleaseShort -- ^ Standard release path following Capistrano's format
  | ReleaseLong  -- ^ Long release path including picoseconds
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON ReleaseFormat where
  parseJSON = withText "release format" $ \t ->
    case t of
      "short" -> return ReleaseShort
      "long"  -> return ReleaseLong
      _       -> fail "expected 'short' or 'long'"

-- | Current shells supported.
data Shell =
  Bash
  | Zsh
  deriving (Show, Eq, Ord)

instance FromJSON Shell where
  parseJSON = withText "shell" $ \t ->
    case t of
      "bash" -> return Bash
      "zsh"  -> return Zsh
      _       -> fail "supported shells: 'bash' or 'zsh'"

-- | SSH options.

data SshOptions = SshOptions
  { sshHost :: String  -- ^ Host to use
  , sshPort :: Word    -- ^ Port to use
  } deriving (Show, Read, Eq, Ord)

-- | Output destination.

data OutputDest
  = StdoutDest
  | StderrDest
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

-- | Release indentifier.

data Release = Release ReleaseFormat UTCTime
  deriving (Eq, Show, Ord)

-- | Target's system where application will be deployed

data TargetSystem
  = GNULinux
  | BSD
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

-- | Create a 'Release' indentifier.

mkRelease :: ReleaseFormat -> UTCTime -> Release
mkRelease = Release

-- | Extract deployment time from 'Release'.

releaseTime :: Release -> UTCTime
releaseTime (Release _ time) = time

-- | Render 'Release' indentifier as a 'String'.

renderRelease :: Release -> String
renderRelease (Release rfmt time) = formatTime defaultTimeLocale fmt time
  where
    fmt = case rfmt of
      ReleaseShort -> releaseFormatShort
      ReleaseLong  -> releaseFormatLong

-- | Parse 'Release' identifier from a 'String'.

parseRelease :: String -> Maybe Release
parseRelease s = (Release ReleaseLong <$> p releaseFormatLong s)
  <|> (Release ReleaseShort <$> p releaseFormatShort s)
  where
    p = parseTimeM False defaultTimeLocale

releaseFormatShort, releaseFormatLong :: String
releaseFormatShort = "%Y%m%d%H%M%S"
releaseFormatLong  = "%Y%m%d%H%M%S%q"

-- | Get release format based on the CLI and file configuration values.

fromMaybeReleaseFormat :: Maybe ReleaseFormat -> Maybe ReleaseFormat -> ReleaseFormat
fromMaybeReleaseFormat cliRF configRF = fromMaybe ReleaseShort (cliRF <|> configRF)

-- | Get keep releases based on the CLI and file configuration values.

fromMaybeKeepReleases :: Maybe Natural -> Maybe Natural -> Natural
fromMaybeKeepReleases cliKR configKR = fromMaybe defaultKeepReleases (cliKR <|> configKR)

defaultKeepReleases :: Natural
defaultKeepReleases = 5
