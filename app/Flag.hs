module Flag where

import Data.Monoid ((<>))
import Options.Applicative

data Flag
  = Version
  deriving Show

flags :: Parser Flag
flags =
  flag' Version (long "version" <> short 'v' <> help "Diplay the version of Hapistrano")
