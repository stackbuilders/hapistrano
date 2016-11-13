module Options (
  Option(..)
  , opts
  , hapistranoDesc
  -- | Imports from Options.Applicative
  , execParser
  , info
  , helper
  )
  where

import Options.Applicative

-- | Flags and commands
opts :: Parser Option
opts
  = flags
  <|> commands

flags :: Parser Option
flags =
  flag' Version (long "version" <> short 'v' <> help "Diplay the version of Hapistrano")

data Option
  = Deploy
  | Rollback
  | Version
  deriving Show

addCommand :: Option -> String -> String -> Mod CommandFields Option
addCommand command' name description =
  command name (info (pure command') (progDesc description))

commands :: Parser Option
commands
  = subparser
    (
    addCommand Deploy "deploy" "Deploys the current release with the configure options"
    <> addCommand Rollback "rollback" "Rolls back to the previous release"
    )

hapistranoDesc :: InfoMod a
hapistranoDesc =
  fullDesc
    <> header "Hapistrano - A deployment library for Haskell applications"
    <> progDesc "Deploy tool for Haskell applications"
    <> footer "Run 'hap -h' for available commands"

