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
    it "creates a cache repo" $
      withDeployTo $ \deployTo -> do
        deploy $ config deployTo
        doesDirectoryExist (unRepoPath $ getRepoPath deployTo) `shouldReturn` True

    it "symlinks the latest release on the current directory" $
      withDeployTo $ \deployTo -> do
        deploy $ config deployTo
        isSymbolicLink (unCurrentPath $ getCurrentPath deployTo) `shouldReturn` True

withDeployTo :: (DeployTo -> IO a) -> IO a
withDeployTo f = withSystemTempDirectory "hapistrano" (f . DeployTo)

config :: DeployTo -> Config
config deployTo =
  Config
    { configDeployTo = deployTo
    , configRepoUrl = repoUrl
    , configKeepReleases = def
    }

repoUrl :: URL
repoUrl = fromJust $ importURL "https://github.com/stackbuilders/hapistrano"
