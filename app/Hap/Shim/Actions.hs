module Hap.Actions where

import           Hap.Shared.Utils (errorHandler, readConfiguration)

readConfigAndShim :: FilePath -> IO ()
readConfigAndShim configFile = do
  eitherConfig <- readConfiguration configFile
  case eitherConfig of
    Left _ -> errorHandler "The configuration file could not be properly parsed"
    Right config -> void $ shimDeploy config

shimDeploy :: a -> a
shimDeploy = undefined
