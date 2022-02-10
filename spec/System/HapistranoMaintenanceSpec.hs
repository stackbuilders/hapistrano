module System.HapistranoMaintenanceSpec (
  spec
) where

import Test.Hspec
import System.Directory
import System.Hapistrano.Maintenance

spec :: Spec
spec =
  around withSandbox $ do
    describe "writeMaintenanceFile" $
      context "when the file doesn't exist" $
        it "creates the maintenance file in the given path" $ \(deployPath, repoPath) -> do
          runHap $ do
            writeMaintenanceFile $(mkRelDir "maintenance") "maintenance.html"
            doesFileExist (deployPath <> "/maintenance/maintenance.html")
