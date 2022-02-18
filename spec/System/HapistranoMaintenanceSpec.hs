{-# LANGUAGE TemplateHaskell #-}

module System.HapistranoMaintenanceSpec (
  spec
) where

import Test.Hspec hiding (shouldBe)
import System.Directory
import System.Hapistrano.Maintenance
import System.Utils
import Path
import Control.Monad.IO.Class
spec :: Spec
spec =
  around withSandbox $
    describe "writeMaintenanceFile" $
      context "when the file doesn't exist" $
        it "creates the maintenance file in the given path" $ \(deployPath, _) -> do
          result <- runHap $ do
            writeMaintenanceFile $(mkRelDir "maintenance") "maintenance.html"
            liftIO $ doesFileExist ((fromAbsDir deployPath) <> "/maintenance/maintenance.html")
          result `shouldBe` True
-- Move this to utils and import it here
-- Make sure the test passes

-- | Lifted 'Hspec.shouldBe'.
shouldBe :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe x y = liftIO (x `shouldBe` y)
