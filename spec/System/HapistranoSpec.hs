module System.HapistranoSpec (spec) where

import Test.Hspec (it, describe, shouldBe, Spec)

import System.IO.Temp (withSystemTempDirectory)

import System.Directory (getDirectoryContents)
import Control.Monad (void, replicateM_)
import System.FilePath.Posix (joinPath)

import qualified System.Hapistrano as Hap
import System.Hapistrano.Types
import Data.List (sort)
import System.Process (readProcessWithExitCode)

runCommand :: String -> IO ()
runCommand command = do
  let cmdParts = words command
      (cmd, args) = (head cmdParts, tail cmdParts)

  putStrLn $ "GIT running: " ++ command
  res <- readProcessWithExitCode cmd args ""
  putStrLn $ "GIT res: " ++ show res

-- ^ Generate a source git repo as test fixture. Push an initial commit
-- to the bare repo by making a clone and committing a trivial change and
-- pushing to the bare repo.
genSourceRepo :: FilePath -> IO FilePath
genSourceRepo path = do
  let fullRepoPath = joinPath [path, "testRepo"]
      clonePath    = joinPath [path, "testRepoClone"]

      commands =
        [ "mkdir -p " ++ fullRepoPath
        , "git init --bare " ++ fullRepoPath
        , "git clone " ++ fullRepoPath ++ " " ++ clonePath
        , "echo testing > " ++ joinPath [clonePath, "README"]
        , "cd " ++ clonePath ++ " && git add -A"
        , "cd " ++ clonePath ++ " && git commit -m\"First commit\""
        , "cd " ++ clonePath ++ " && git push"
        ]

  mapM_ runCommand commands

  return fullRepoPath

rollback :: Hap.Config -> IO ()
rollback cfg =
  Hap.runRC errorHandler successHandler cfg $ do

    _ <- Hap.rollback
    void Hap.restartServerCommand

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler


-- | Deploys the current release with Config options.
deployOnly :: Hap.Config -> IO ()
deployOnly cfg =
  Hap.runRC errorHandler successHandler cfg $ void Hap.pushRelease

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler

-- | Deploys the current release with Config options.
deployAndActivate :: Hap.Config -> IO ()
deployAndActivate cfg =
  Hap.runRC errorHandler successHandler cfg $ do
    rel <- Hap.pushRelease
    _ <- Hap.runBuild rel

    void $ Hap.activateRelease rel

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler

defaultState :: FilePath -> FilePath -> Hap.Config
defaultState tmpDir testRepo =
  Hap.Config { Hap.deployPath     = tmpDir
             , Hap.host           = Nothing
             , Hap.repository     = testRepo
             , Hap.releaseFormat  = Long
             , Hap.revision       = "master"
             , Hap.buildScript    = Nothing
             , Hap.restartCommand = Nothing
             }

spec :: Spec
spec = describe "hapistrano" $ do
  describe "readCurrentLink" $
    it "trims trailing whitespace" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do

        testRepoPath <- genSourceRepo tmpDir

        deployAndActivate $ defaultState tmpDir testRepoPath

        ltarget <- Hap.readCurrentLink Nothing (Hap.currentPath tmpDir)

        last ltarget /= '\n' `shouldBe` True

  describe "deploying" $ do
    it "a simple deploy" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do

        testRepoPath <- genSourceRepo tmpDir

        deployOnly $ defaultState tmpDir testRepoPath

        contents <- getDirectoryContents (joinPath [tmpDir, "releases"])
        length (filter (Hap.isReleaseString Long) contents) `shouldBe` 1

    it "activates the release" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do

        testRepoPath <- genSourceRepo tmpDir

        deployAndActivate $ defaultState tmpDir testRepoPath

        contents <- getDirectoryContents (joinPath [tmpDir, "releases"])
        length (filter (Hap.isReleaseString Long) contents) `shouldBe` 1

    it "cleans up old releases" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do
        testRepoPath <- genSourceRepo tmpDir

        replicateM_ 7 $ deployAndActivate $ defaultState tmpDir testRepoPath

        contents <- getDirectoryContents (joinPath [tmpDir, "releases"])
        length (filter (Hap.isReleaseString Long) contents) `shouldBe` 5

  describe "rollback" $
    it "rolls back to the previous release" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do

        testRepoPath <- genSourceRepo tmpDir
        let deployState = defaultState tmpDir testRepoPath

        deployAndActivate deployState

        -- current symlink should point to the last release directory
        contents <- getDirectoryContents (joinPath [tmpDir, "releases"])

        let firstRelease = head $ filter (Hap.isReleaseString Long) contents

        firstReleaseLinkTarget <-
          Hap.readCurrentLink Nothing (Hap.currentPath tmpDir)

        putStrLn $ "the first: " ++ show firstReleaseLinkTarget
        firstRelease `shouldBe` Hap.pathToRelease firstReleaseLinkTarget

        -- deploy a second version
        deployAndActivate deployState

        -- current symlink should point to second release

        conts <- getDirectoryContents (joinPath [tmpDir, "releases"])

        let secondRelease =
              sort (filter (Hap.isReleaseString Long) conts) !! 1

        secondReleaseLinkTarget <-
          Hap.readCurrentLink Nothing (Hap.currentPath tmpDir)

        secondRelease `shouldBe` Hap.pathToRelease secondReleaseLinkTarget

        -- roll back, and current symlink should point to first release again

        rollback deployState

        afterRollbackLinkTarget <-
          Hap.readCurrentLink Nothing (Hap.currentPath tmpDir)

        Hap.pathToRelease afterRollbackLinkTarget `shouldBe` firstRelease
