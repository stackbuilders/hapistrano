-- |
-- Module      :  System.Hapistrano.Types
-- Copyright   :  © 2015-2017 Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Type definitions for the Hapistrano tool.

module System.Hapistrano.Types
  ( Hapistrano
  , Failure (..)
  , Config (..)
  , Task (..)
  , ReleaseFormat(..)
  , SshOptions (..)
  , Release
  , mkRelease
  , releaseTime
  , renderRelease
  , parseRelease )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Time
import Path

-- | Hapistrano monad.

type Hapistrano a = ExceptT Failure (ReaderT Config IO) a

-- | Failure with status code and a message.

data Failure = Failure Int (Maybe String)

-- | Hapistrano configuration options.

data Config = Config
  { configSshOptions :: Maybe SshOptions
    -- ^ 'Nothing' if we are running locally, or SSH options to use.
  }

-- | The records describes deployment task.

data Task = Task
  { taskDeployPath :: Path Abs Dir
    -- ^ The root of the deploy target on the remote host
  , taskRepository :: String
    -- ^ The URL of remote Git repo to deploy
  , taskRevision :: String
    -- ^ A SHA1 or branch to release
  , taskReleaseFormat :: ReleaseFormat
    -- ^ The 'ReleaseFormat' to use
  } deriving (Show, Eq)

-- | Release format mode.

data ReleaseFormat
  = ReleaseShort -- ^ Standard release path following Capistrano's format
  | ReleaseLong  -- ^ Long release path including picoseconds
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | SSH options.

data SshOptions = SshOptions
  { sshHost :: String  -- ^ Host to use
  , sshPort :: Word    -- ^ Port to use
  } deriving (Show, Eq, Ord)

-- | Release indentifier.

data Release = Release ReleaseFormat UTCTime
  deriving (Eq, Show, Ord)

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
