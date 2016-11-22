module Hap.Actions where

import           Control.Monad (void)
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS
import           Hapistrano.Deploy (deploy)
import           System.IO (hPutStrLn, stderr)
import           System.Exit (ExitCode(..), exitWith)


readConfigAndDeploy :: FilePath -> IO ()
readConfigAndDeploy configFile = do
  eitherConfig <- readConfiguration configFile
  case eitherConfig of
    Left _ -> errorHandler "The configuration file could not be properly parsed"
    Right config -> void $ deploy config
  where
    readConfiguration f = fmap eitherDecode $ BS.readFile f

errorHandler :: String -> IO ()
errorHandler msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)
