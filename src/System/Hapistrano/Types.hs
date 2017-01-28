module System.Hapistrano.Types
       ( Config(..)
       , FailureResult
       , Hapistrano
       , Release
       , ReleaseFormat(..)
       ) where

import Control.Monad.Reader (ReaderT(..))
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

         , port :: Maybe Integer
           -- ^ Optional port to deploy to a different ssh port

         } deriving (Show)

data ReleaseFormat = Short
                     -- ^ Standard release path following Capistrano's format

                   | Long
                     -- ^ Long release path including picoseconds for testing
                     -- or people seriously into continuous deployment

                   deriving (Show)


type Release = String

type FailureResult = (Int, String)

type Hapistrano a = EitherT FailureResult (ReaderT Config IO) a
