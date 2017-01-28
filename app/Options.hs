module Options (
  Option(..)
  , opts
  , hapistranoDesc
  -- | Imports from Options.Applicative
  , execParser
  , info
  , helper
  -- | Imports from other internal modules
  , module Command
  , module Flag
  )
  where

import Command
import Flag

import Data.Monoid ((<>))
import Options.Applicative

-- | Flags and commands
opts :: Parser Option
opts
  = fmap Flag flags
  <|> fmap Command commands

data Option
  = Command Command.Command
  | Flag Flag.Flag
  deriving Show

hapistranoDesc :: InfoMod a
hapistranoDesc =
  fullDesc
    <> header "Hapistrano - A deployment library for Haskell applications"
    <> progDesc "Deploy tool for Haskell applications"
    <> footer "Run 'hap -h' for available commands"
