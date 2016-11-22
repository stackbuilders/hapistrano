module Main where

import Hap.Actions
import Hap.Options

import Options.Applicative

main :: IO ()
main = execParser argumentsParser >>= hapCLI

hapCLI :: HapOptions -> IO ()
hapCLI (Deploy configFile) = readConfigAndDeploy configFile

argumentsParser :: ParserInfo HapOptions
argumentsParser = info (helper <*> hapOptions)
                    (fullDesc
                    <> header "Hapistrano is a deployment library for Haskell applications"
                    <> progDesc "CLI utility to deploy projects in a similar way to Capistrano"
                    <> footer "Run 'new-hap -h' for available commands"
                    )
