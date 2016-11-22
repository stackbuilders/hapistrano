module Hap.Shim.Options where

import Options.Applicative

data ShimOptions = ExecuteShim FilePath

shimOptions :: Parser ShimOptions
shimOptions = ExecuteShim <$> parseShimCommand

parseShimCommand :: Parser FilePath
parseHapCommand = subparser $
  command "deploy" (info deployToHost (progDesc "Runs the deployment in the remote host with a configuration file"))

deployToHost :: Parser FilePath
deployToHost = argument str (metavar "<path to configuration file>")
