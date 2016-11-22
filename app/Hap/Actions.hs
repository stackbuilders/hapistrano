module Hap.Actions where

import           Control.Monad (void)
import           Hapistrano.Deploy (deploy)
import           Hap.Shared.Utils (errorHandler, readConfiguration)

readConfigAndDeploy :: FilePath -> IO ()
readConfigAndDeploy configFile = do
  eitherConfig <- readConfiguration configFile
  case eitherConfig of
    Left _ -> errorHandler "The configuration file could not be properly parsed"
    Right config -> void $ deploy config
