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
      withDeployPath $ \deployPath -> do
        deploy $ config deployPath
        doesDirectoryExist (unRepoPath $ getRepoPath deployPath) `shouldReturn` True

    it "symlinks the latest release on the current directory" $
      withDeployPath $ \deployPath -> do
        deploy $ config deployPath
        isSymbolicLink (unCurrentPath $ getCurrentPath deployPath) `shouldReturn` True

withDeployPath :: (DeployPath -> IO a) -> IO a
withDeployPath f = withSystemTempDirectory "hapistrano" (f . DeployPath)

config :: DeployPath -> Config
config deployPath =
  Config
    { configDeployPath = deployPath
    , configRepoUrl = repoUrl
    , configKeepReleases = def
    }

repoUrl :: URL
repoUrl = fromJust $ importURL "https://github.com/stackbuilders/hapistrano"
