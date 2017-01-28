module Command where

import Options.Applicative

data Command
  = Deploy
  | Rollback
  deriving Show

addCommand :: Command -> String -> String -> Mod CommandFields Command
addCommand command' name description =
  command name (info (pure command') (progDesc description))

commands :: Parser Command
commands
  = subparser
    (
    addCommand Deploy "deploy" "Deploys the current release with the configure options"
    <> addCommand Rollback "rollback" "Rolls back to the previous release"
    )

