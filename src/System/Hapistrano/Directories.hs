module System.Hapistrano.Directories where

import           Control.Monad.Reader
--import           Data.Text            (Text)
--import qualified Data.Text as T (unpack)
import           Development.Shake

{-
main' :: IO ()
main' = shake shakeOptions $ do
  setupDirs $ RemoteHost "welly" 
-}


data Config = undefined

ensureRepositoryPushed :: Config -> Either String String 
ensureRepositoryPushed = undefined 
