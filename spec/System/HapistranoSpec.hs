module System.HapistranoSpec (spec) where

import Test.Hspec (it, describe, shouldBe, Spec)

import System.IO.Temp (withSystemTempDirectory)

import System.Directory (getDirectoryContents)
import Control.Monad (void, replicateM_)
import Control.Monad.Trans.Either (runEitherT)

import Control.Monad.Reader (ReaderT(..))

import System.FilePath.Posix (joinPath)

import qualified System.Hapistrano as Hap
import Data.List (intercalate, sort)

import qualified System.IO as IO
import qualified System.Process as Process

runCommand :: String -> IO ()
runCommand command = do
  putStrLn ("GIT running: " ++ command)
  let process = Process.shell command
  (_, Just outHandle, Just errHandle, processHandle) <-
    Process.createProcess process { Process.std_err = Process.CreatePipe
                                  , Process.std_in = Process.CreatePipe
                                  , Process.std_out = Process.CreatePipe
                                  }
  exitCode <- fmap show (Process.waitForProcess processHandle)
  out <- IO.hGetContents outHandle
  err <- IO.hGetContents errHandle
  putStrLn ("GIT res: " ++ show (exitCode, out, err))

-- | Generate a source git repo as test fixture. Push an initial commit
-- to the bare repo by making a clone and committing a trivial change and
-- pushing to the bare repo.
genSourceRepo :: FilePath -> IO FilePath
genSourceRepo path = do
  let fullRepoPath = joinPath [path, "testRepo"]
      clonePath    = joinPath [path, "testRepoClone"]

      gitConfigReplace =
        intercalate
          " && "
          [ "git config --local --replace-all push.default simple"
          , "git config --local --replace-all user.email hap@hap"
          , "git config --local --replace-all user.name Hap"
          ]

      gitConfigUnset =
        intercalate
          " && "
          [ "git config --local --unset push.default"
          , "git config --local --unset user.email"
          , "git config --local --unset user.name"
          ]

      commands =
        [ "mkdir -p " ++ fullRepoPath
        , "git init --bare " ++ fullRepoPath
        , "git clone " ++ fullRepoPath ++ " " ++ clonePath
        , "echo testing > " ++ joinPath [clonePath, "README"]
        , "cd " ++ clonePath ++ " && " ++ gitConfigReplace
        , "cd " ++ clonePath ++ " && git add -A"
        , "cd " ++ clonePath ++ " && git commit -m\"First commit\""
        , "cd " ++ clonePath ++ " && git push"
        , "cd " ++ clonePath ++ " && " ++ gitConfigUnset
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
             , Hap.releaseFormat  = Hap.Long
             , Hap.revision       = "master"
             , Hap.buildScript    = Nothing
             , Hap.restartCommand = Nothing
             }

-- | The 'fromRight' function extracts the element out of a 'Right' and
-- throws an error if its argument take the form  @Left _@.
fromRight           :: Either a b -> b
fromRight (Left _)  = error "fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x

spec :: Spec
spec = describe "hapistrano" $ do
  describe "readCurrentLink" $
    it "trims trailing whitespace" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do

        testRepoPath <- genSourceRepo tmpDir

        deployAndActivate $ defaultState tmpDir testRepoPath

        ltarget <-
          runReaderT (runEitherT Hap.readCurrentLink) $
          defaultState tmpDir testRepoPath

        last (fromRight ltarget) /= '\n' `shouldBe` True

  describe "deploying" $ do
    it "a simple deploy" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do

        testRepoPath <- genSourceRepo tmpDir

        deployOnly $ defaultState tmpDir testRepoPath

        contents <- getDirectoryContents (joinPath [tmpDir, "releases"])
        length (filter (Hap.isReleaseString Hap.Long) contents) `shouldBe` 1

    it "activates the release" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do

        testRepoPath <- genSourceRepo tmpDir

        deployAndActivate $ defaultState tmpDir testRepoPath

        contents <- getDirectoryContents (joinPath [tmpDir, "releases"])
        length (filter (Hap.isReleaseString Hap.Long) contents) `shouldBe` 1

    it "cleans up old releases" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do
        testRepoPath <- genSourceRepo tmpDir

        replicateM_ 7 $ deployAndActivate $ defaultState tmpDir testRepoPath

        contents <- getDirectoryContents (joinPath [tmpDir, "releases"])
        length (filter (Hap.isReleaseString Hap.Long) contents) `shouldBe` 5

  describe "rollback" $
    it "rolls back to the previous release" $
      withSystemTempDirectory "hapistranoDeployTest" $ \tmpDir -> do

        testRepoPath <- genSourceRepo tmpDir
        let deployState = defaultState tmpDir testRepoPath

        deployAndActivate deployState

        -- current symlink should point to the last release directory
        contents <- getDirectoryContents (joinPath [tmpDir, "releases"])

        let firstRelease = head $ filter (Hap.isReleaseString Hap.Long) contents

        firstReleaseLinkTarget <-
          runReaderT (runEitherT Hap.readCurrentLink) deployState

        firstRelease `shouldBe` Hap.pathToRelease (fromRight firstReleaseLinkTarget)

        -- deploy a second version
        deployAndActivate deployState

        -- current symlink should point to second release

        conts <- getDirectoryContents (joinPath [tmpDir, "releases"])

        let secondRelease =
              sort (filter (Hap.isReleaseString Hap.Long) conts) !! 1

        secondReleaseLinkTarget <-
          runReaderT (runEitherT Hap.readCurrentLink) deployState

        secondRelease `shouldBe` Hap.pathToRelease (fromRight secondReleaseLinkTarget)

        -- roll back, and current symlink should point to first release again

        rollback deployState

        afterRollbackLinkTarget <-
          runReaderT (runEitherT Hap.readCurrentLink) deployState

        Hap.pathToRelease (fromRight afterRollbackLinkTarget) `shouldBe` firstRelease
