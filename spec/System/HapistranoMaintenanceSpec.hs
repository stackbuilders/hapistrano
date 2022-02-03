import Test.Hspec
import System.Directory
module System.HapistranoMaintenanceSpec (

) where

spec :: Spec
spec =
  around withSandbox $ do
    describe "writeMaintenanceFile" $
      context "when the file doesn't exist" $
        it "creates the maintenance file in the given path" $
          runHap $ do
            writeMaintenanceFile $(mkRelDir "maintenance") "maintenance.html"
            doesFileExist "/maintenance/maintenance.html"
