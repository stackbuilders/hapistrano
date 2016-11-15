module System.Hapistrano.Helpers where

import Data.Text (Text)
import qualified Data.Text as T

data Config = Config {
  deployPath :: Text,
  host :: Text,
  repository :: Text,
  revision :: Text,
  buildScript :: Text,
  restartCmd :: Text
  } deriving (Eq, Show)

ensureRepositoryPushed :: Config -> Either String String 
ensureRepositoryPushed = undefined 
