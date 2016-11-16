{-# LANGUAGE RecordWildCards #-}

module HapistranoSpec where

import           Data.Default
import           Data.Maybe
import           Network.URL
import           System.Directory
import           System.IO.Temp
import           Test.Hspec

import           Hapistrano

spec :: Spec
spec =
  describe "deploy" $ do
    it "symlinks the current directory" $
      withConfig $ \config@Config{..} -> do
        deploy config
        currentPath <- getCurrentPath configDeployPath
        isSymbolicLink currentPath `shouldReturn` True

    it "creates a cache repo" $
      withConfig $ \config@Config{..} -> do
        deploy config
        repoPath <- getRepoPath configDeployPath
        doesDirectoryExist repoPath `shouldReturn` True

withConfig :: (Config -> IO a) -> IO a
withConfig f = withSystemTempDirectory "hapistrano" (f . getConfig)

getConfig :: FilePath -> Config
getConfig deployPath =
  Config
    { configDeployPath = deployPath
    , configRepoUrl = getRepoUrl
    , configKeepReleases = def
    }

getRepoUrl :: URL
getRepoUrl = fromJust $ importURL "https://github.com/stackbuilders/hapistrano"
