module HapistranoSpec where

import           Data.Default
import           Data.Maybe
import           Network.URL
import           System.IO.Temp
import           Test.Hspec

import           Hapistrano

spec :: Spec
spec =
  describe "deploy" $
    it "..." $ withDeployTo $ \deployTo -> do
      deploy $ config deployTo
      pending

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
