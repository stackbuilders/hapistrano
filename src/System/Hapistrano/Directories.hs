{-# LANGUAGE OverloadedStrings #-}

module System.Hapistrano.Directories where

import           Control.Monad.Reader
import           Data.Text            (Text)
import           Development.Shake

newtype RemoteHost = RemoteHost { unRemoteHost :: Text }
  deriving (Eq, Show)

main :: IO ()
main = shake shakeOptions $ do
  setupDirs


setupDirs :: Rules ()
setupDirs =
  action $ do
    unit $ cmd "ssh server mkdir -p repo"
    unit $ cmd "ssh server mkdir -p releases"

myCmd :: Text -> [Text] -> ReaderT RemoteHost Action a
myCmd _ _ = do
  hostName <- asks unRemoteHost
  undefined
