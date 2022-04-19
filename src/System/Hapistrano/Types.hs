-- |
-- Module      :  System.Hapistrano.Types
-- Copyright   :  © 2015-Present Stack Builders
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- Type definitions for the Hapistrano tool.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Hapistrano.Types
  ( Hapistrano
  , Failure(..)
  , Config(..)
  , Source(..)
  , Task(..)
  , ReleaseFormat(..)
  , SshOptions(..)
  , OutputDest(..)
  , Release
  , TargetSystem(..)
  , DeployState(..)
  , Shell(..)
  , Opts(..)
  , Command(..)
  , MaintenanceOptions(..)
  -- * Types helpers
  , mkRelease
  , releaseTime
  , renderRelease
  , parseRelease
  , fromMaybeReleaseFormat
  , fromMaybeKeepReleases
  , toMaybePath
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Maybe
import           Data.Time
import           Numeric.Natural
import           Path

-- | Hapistrano monad.
type Hapistrano a = ExceptT (Failure, Maybe Release) (ReaderT Config IO) a

-- | Failure with status code and a message.
data Failure =
  Failure Int (Maybe String)

-- | Hapistrano configuration options.
data Config =
  Config
    { configSshOptions   :: !(Maybe SshOptions)
    -- ^ 'Nothing' if we are running locally, or SSH options to use.
    , configShellOptions :: !Shell
    -- ^ One of the supported 'Shell's
    , configPrint        :: !(OutputDest -> String -> IO ())
    -- ^ How to print messages
    }

-- | The source of the repository. It can be from a version control provider
-- like GitHub or a local directory.
data Source
  = GitRepository
      { gitRepositoryURL      :: String
      -- ^ The URL of remote Git repository to deploy
      , gitRepositoryRevision :: String
      -- ^ The SHA1 or branch to release
      }
  | LocalDirectory
      { localDirectoryPath :: Path Abs Dir
      -- ^ The local repository to deploy
      }
  deriving (Eq, Ord, Show)

-- | The records describes deployment task.
data Task =
  Task
    { taskDeployPath    :: Path Abs Dir
    -- ^ The root of the deploy target on the remote host
    , taskSource        :: Source
    -- ^ The 'Source' to deploy
    , taskReleaseFormat :: ReleaseFormat
    -- ^ The 'ReleaseFormat' to use
    }
  deriving (Show, Eq, Ord)

-- | Release format mode.
data ReleaseFormat
  = ReleaseShort -- ^ Standard release path following Capistrano's format
  | ReleaseLong -- ^ Long release path including picoseconds
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON ReleaseFormat where
  parseJSON =
    withText "release format" $ \case
      "short" -> return ReleaseShort
      "long" -> return ReleaseLong
      _ -> fail "expected 'short' or 'long'"

-- | Current shells supported.
data Shell
  = Bash
  | Zsh
  deriving (Show, Eq, Ord)

instance FromJSON Shell where
  parseJSON =
    withText "shell" $ \case
      "bash" -> return Bash
      "zsh" -> return Zsh
      _ -> fail "supported shells: 'bash' or 'zsh'"

-- | SSH options.
data SshOptions =
  SshOptions
    { sshHost :: String -- ^ Host to use
    , sshPort :: Word -- ^ Port to use
    , sshArgs :: [String] -- ^ Arguments for ssh
    }
  deriving (Show, Read, Eq, Ord)

-- | Output destination.
data OutputDest
  = StdoutDest
  | StderrDest
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

-- | Release indentifier.
data Release =
  Release ReleaseFormat UTCTime
  deriving (Eq, Show, Ord)

-- | Target's system where application will be deployed.
data TargetSystem
  = GNULinux
  | BSD
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

-- | State of the deployment after running @hap deploy@.
-- __note:__ the 'Unknown' value is not intended to be
-- written to the @.hapistrano_deploy_state@ file; instead,
-- it's intended to represent whenever Hapistrano couldn't
-- get the information on the deployment state (e.g. the file is not present).
data DeployState
  = Fail
  | Success
  | Unknown
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

-- | Maintenance options

data MaintenanceOptions = Enable | Disable

-- | Command line options.

data Opts = Opts
  { optsCommand    :: Command
  , optsConfigFile :: FilePath
  }

-- | Command to execute and command-specific options.

data Command
  = Deploy (Maybe ReleaseFormat) (Maybe Natural) Bool -- ^ Deploy a new release (with timestamp
    -- format, how many releases to keep, and whether the failed releases except the latest one
    -- get deleted or not)
  | Rollback Natural -- ^ Rollback to Nth previous release
  | Maintenance MaintenanceOptions

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
    fmt =
      case rfmt of
        ReleaseShort -> releaseFormatShort
        ReleaseLong  -> releaseFormatLong

----------------------------------------------------------------------------
-- Types helpers

-- | Parse 'Release' identifier from a 'String'.
parseRelease :: String -> Maybe Release
parseRelease s =
  (Release ReleaseLong <$> p releaseFormatLong s) <|>
  (Release ReleaseShort <$> p releaseFormatShort s)
  where
    p = parseTimeM False defaultTimeLocale

releaseFormatShort, releaseFormatLong :: String
releaseFormatShort = "%Y%m%d%H%M%S"

releaseFormatLong = "%Y%m%d%H%M%S%q"

-- | Get release format based on the CLI and file configuration values.
fromMaybeReleaseFormat ::
     Maybe ReleaseFormat -> Maybe ReleaseFormat -> ReleaseFormat
fromMaybeReleaseFormat cliRF configRF =
  fromMaybe ReleaseShort (cliRF <|> configRF)

-- | Get keep releases based on the CLI and file configuration values.
fromMaybeKeepReleases :: Maybe Natural -> Maybe Natural -> Natural
fromMaybeKeepReleases cliKR configKR =
  fromMaybe defaultKeepReleases (cliKR <|> configKR)

defaultKeepReleases :: Natural
defaultKeepReleases = 5

-- | Get the local path to copy from the 'Source' configuration value.
toMaybePath :: Source -> Maybe (Path Abs Dir)
toMaybePath (LocalDirectory path) = Just path
toMaybePath _                     = Nothing
