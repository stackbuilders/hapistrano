module System.Hapistrano.Types
       ( Config(..)
       , HapistranoState(..)
       , RC
       , Release
       , ReleaseFormat(..)
       ) where

import Control.Monad.Trans.State (StateT)

import Control.Monad.Trans.Either (EitherT(..))

-- | Config stuff that will be replaced by config file reading
data Config =
  Config { deployPath     :: String
           -- ^ The root of the deploy target on the remote host

         , repository     :: String -- ^ The remote git repo
         , revision       :: String -- ^ A SHA1 or branch to release

         , releaseFormat  :: ReleaseFormat
         , host           :: Maybe String
           -- ^ The target host for the deploy, or Nothing to indicate that
           -- operations should be done directly in the local deployPath without
           -- going over SSH

         , buildScript    :: Maybe FilePath
           -- ^ The local path to a file that should be executed on the remote
           -- server to build the application.

         , restartCommand :: Maybe String
           -- ^ Optional command to restart the server after a successful deploy

         } deriving (Show)

data ReleaseFormat = Short -- ^ Standard release path following Capistrano
                   | Long  -- ^ Long release path including picoseconds for testing or people seriously into continuous deployment
                   deriving (Show)

data HapistranoState = HapistranoState { config    :: Config
                                       , timestamp :: Maybe String
                                       } deriving Show

type Release = String

type RC a = StateT HapistranoState (EitherT (Int, Maybe String) IO) a
