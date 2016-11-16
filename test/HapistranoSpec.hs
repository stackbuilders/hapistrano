{-# LANGUAGE RecordWildCards #-}

module HapistranoSpec where

import           Control.Monad
import           Data.Default
import           Data.Maybe
import           Development.Shake          (Verbosity (..))
import           Development.Shake.FilePath
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
        void $ deploy config
        currentPath <- getCurrentPath configDeployPath
        isSymbolicLink currentPath `shouldReturn` True

    it "creates a cache repo" $
      withConfig $ \config@Config{..} -> do
        void $ deploy config
        repoPath <- getRepoPath configDeployPath
        doesDirectoryExist repoPath `shouldReturn` True

    it "creates a new release" $
      withConfig $ \config@Config{..} -> do
        releasePath <- deploy config
        doesDirectoryExist releasePath `shouldReturn` True

    it "cleanup the old releases" $
      withConfig $ \config@Config{..} -> do
        replicateM_ 2 $ deploy config { configKeepReleases = KeepReleases 1 }
        releasesPath <- getReleasesPath configDeployPath
        releases <- listDirectory releasesPath
        length releases `shouldBe` 1

withConfig :: (Config -> IO a) -> IO a
withConfig f = withSystemTempDirectory "hapistrano" $ \deployPath -> do
  let sharedPath = deployPath </> "shared"
  createDirectory sharedPath
  copyFile "test/build.sh" (sharedPath </> "build.sh")
  f $ getConfig deployPath

getConfig :: FilePath -> Config
getConfig deployPath =
  Config
    { configBranch = def
    , configDeployPath = deployPath
    , configKeepReleases = def
    , configLinkedFiles = ["build.sh"]
    , configLogLevel = LogLevel Silent
    , configRepoUrl = getRepoUrl
    , configScriptPath = "build.sh"
    }

getRepoUrl :: RepoUrl
getRepoUrl =
  RepoUrl $ fromJust $ importURL "https://github.com/stackbuilders/hapistrano"
