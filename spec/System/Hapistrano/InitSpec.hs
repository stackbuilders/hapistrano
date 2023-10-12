module System.Hapistrano.InitSpec (spec) where

import Test.Hspec
import System.Directory (doesFileExist, getCurrentDirectory, withCurrentDirectory)
import System.FilePath ((</>))
import System.Hapistrano (initConfig)
import System.IO.Temp (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "initConfig" $ do
    it "should create a file when missing" $
      withSystemTempDirectory "hapistrano-spec-initConfig-missing" $ \tempDir ->
        withCurrentDirectory tempDir $ do
          configFilePath <- (</> "hap.yml") <$> getCurrentDirectory
          initConfig $ return ""
          doesFileExist configFilePath `shouldReturn` True
