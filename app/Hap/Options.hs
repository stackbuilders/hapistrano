module Hap.Options where

import           Options.Applicative

data HapOptions = Deploy FilePath

hapOptions :: Parser HapOptions
hapOptions = Deploy <$> parseHapCommand

parseHapCommand :: Parser FilePath
parseHapCommand = subparser $
  command "deploy" (info deployProject (progDesc "Deploys the project to a server"))

deployProject :: Parser FilePath
deployProject = argument str (metavar "<path to configuration file>")
