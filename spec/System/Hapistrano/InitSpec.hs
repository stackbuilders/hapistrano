{-# LANGUAGE LambdaCase #-}

module System.Hapistrano.InitSpec (spec) where

import Data.List (isInfixOf)

import Test.Hspec

import System.Directory (doesFileExist, getCurrentDirectory, withCurrentDirectory)
import System.FilePath ((</>))
import System.Hapistrano.Internal (initConfig')
import System.IO.Temp (withSystemTempDirectory)

getLine' :: String -> IO String
getLine' = return . go
  where
    go s
      | "repo" `isInfixOf` s = show "git@github.com:stackbuilders/hapistrano.git"
      | "revision" `isInfixOf` s = show  "origin/master"
      | "host" `isInfixOf` s = show "user@localhost.com"
      | "port" `isInfixOf` s = "22"
      | "restart Command" `isInfixOf` s = show "n"
      | otherwise = show s

spec :: Spec
spec =
  describe "initConfig" $ do
    it "should create a file when missing" $
      withSystemTempDirectory "hapistrano-spec-initConfig-missing" $ \tempDir ->
        withCurrentDirectory tempDir $ do
          configFilePath <- (</> "hap.yml") <$> getCurrentDirectory
          initConfig' $ const . return $ ""
          doesFileExist configFilePath `shouldReturn` True

    it "should create a file when missing" $
      withSystemTempDirectory "hapistrano-spec-initConfig-missing" $ \tempDir ->
        withCurrentDirectory tempDir $ do
          configFilePath <- (</> "hap.yml") <$> getCurrentDirectory
          initConfig' getLine'
          content <- readFile configFilePath
          content `shouldContain` "user@localhost.com"
