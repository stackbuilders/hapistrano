{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.HapistranoSpec
  ( spec
  ) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.List                     (isPrefixOf)
import           Data.Maybe                    (mapMaybe)
import           Numeric.Natural
import           Path




import           Path.IO
import           System.Directory              (doesFileExist,
                                                getCurrentDirectory,
                                                listDirectory)
import           System.Hapistrano             (releasePath)
import qualified System.Hapistrano             as Hap
import qualified System.Hapistrano.Commands    as Hap
import           System.Hapistrano.Config      (deployStateFilename)
import qualified System.Hapistrano.Core        as Hap
import           System.Hapistrano.Maintenance
import           System.Hapistrano.Types
import           System.Info                   (os)
import           System.IO
import           System.IO.Silently            (capture_)
import           Test.Hspec                    hiding (shouldBe, shouldContain,
                                                shouldReturn)
import qualified Test.Hspec                    as Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck               hiding (Success)

testBranchName :: String
testBranchName = "another_branch"

workingDir :: Path Rel Dir
workingDir = $(mkRelDir "working_dir")

releaseDir :: Path Rel Dir
releaseDir = $(mkRelDir "releases")

spec :: Spec
spec = do
  describe "execWithInheritStdout" $
    context "given a command that prints to stdout" $
    it "redirects commands' output to stdout first" $
    let (Just commandTest) =
          Hap.mkGenericCommand
            "echo \"hapistrano\"; sleep 2; echo \"onartsipah\""
        commandExecution = Hap.execWithInheritStdout commandTest Nothing
        expectedOutput = "hapistrano\nonartsipah"
     in do actualOutput <- capture_ (runHap commandExecution)
           expectedOutput `Hspec.shouldSatisfy` (`isPrefixOf` actualOutput)
  describe "readScript" $
    it "performs all the necessary normalizations correctly" $ do
      spath <- do
        currentDirectory <- getCurrentDirectory >>= parseAbsDir
        scriptFile <- parseRelFile "script/clean-build.sh"
        return (currentDirectory </> scriptFile)
      (fmap Hap.unGenericCommand <$> Hap.readScript spath) `Hspec.shouldReturn`
        [ "export PATH=~/.cabal/bin:/usr/local/bin:$PATH"
        , "cabal sandbox delete"
        , "cabal sandbox init"
        , "cabal clean"
        , "cabal update"
        , "cabal install --only-dependencies -j"
        , "cabal build -j"
        ]
  describe "fromMaybeReleaseFormat" $ do
    context "when the command line value is present" $ do
      context "and the config file value is present" $
        prop "returns the command line value" $
        forAll ((,) <$> arbitraryReleaseFormat <*> arbitraryReleaseFormat) $ \(rf1, rf2) ->
          fromMaybeReleaseFormat (Just rf1) (Just rf2) `Hspec.shouldBe` rf1
      context "and the config file value is not present" $
        prop "returns the command line value" $
        forAll arbitraryReleaseFormat $ \rf ->
          fromMaybeReleaseFormat (Just rf) Nothing `Hspec.shouldBe` rf
    context "when the command line value is not present" $ do
      context "and the config file value is present" $
        prop "returns the config file value" $
        forAll arbitraryReleaseFormat $ \rf ->
          fromMaybeReleaseFormat Nothing (Just rf) `Hspec.shouldBe` rf
      context "and the config file value is not present" $
        it "returns the default value" $
        fromMaybeReleaseFormat Nothing Nothing `Hspec.shouldBe` ReleaseShort
  describe "fromMaybeKeepReleases" $ do
    context "when the command line value is present" $ do
      context "and the config file value is present" $
        prop "returns the command line value" $
        forAll ((,) <$> arbitraryKeepReleases <*> arbitraryKeepReleases) $ \(kr1, kr2) ->
          fromMaybeKeepReleases (Just kr1) (Just kr2) `Hspec.shouldBe` kr1
      context "and the second value is not present" $
        prop "returns the command line value" $
        forAll arbitraryKeepReleases $ \kr ->
          fromMaybeKeepReleases (Just kr) Nothing `Hspec.shouldBe` kr
    context "when the command line value is not present" $ do
      context "and the config file value is present" $
        prop "returns the config file value" $
        forAll arbitraryKeepReleases $ \kr ->
          fromMaybeKeepReleases Nothing (Just kr) `Hspec.shouldBe` kr
      context "and the config file value is not present" $
        it "returns the default value" $
        fromMaybeKeepReleases Nothing Nothing `Hspec.shouldBe` 5
  around withSandbox $ do
    describe "writeMaintenanceFile" $
      context "when the file doesn't exist" $
        it "creates the maintenance file in the given path" $ \(deployPath, _) -> do
          result <- runHap $ do
            writeMaintenanceFile deployPath $(mkRelDir "maintenance") $(mkRelFile "maintenance.html")
            liftIO $ System.Directory.doesFileExist ((fromAbsDir deployPath) <> "/maintenance/maintenance.html")
          result `shouldBe` True
    describe "deleteMaintenanceFile" $
      context "when the file exists" $
        it "removes the maintenance file from the given path" $ \(deployPath, _) -> do
          result <- runHap $ do
            writeMaintenanceFile deployPath $(mkRelDir "maintenance") $(mkRelFile "maintenance.html")
            deleteMaintenanceFile deployPath $(mkRelDir "maintenance") $(mkRelFile "maintenance.html")
            liftIO $ System.Directory.doesFileExist ((fromAbsDir deployPath) <> "/maintenance/maintenance.html")
          result `shouldBe` False
    describe "releasePath" $ do
      context "when the configWorkingDir is Nothing" $
        it "should return the release path" $ \(deployPath, repoPath) -> do
          (rpath, release) <- runHap $ do
            release <- Hap.pushRelease $ mkTask deployPath repoPath
            (,) <$> Hap.releasePath deployPath release Nothing
                <*> pure release

          rel <- parseRelDir $ renderRelease release
          rpath `shouldBe` deployPath </> releaseDir </> rel

      context "when the configWorkingDir is Just" $
        it "should return the release path with WorkingDir" $ \(deployPath, repoPath) -> do
          (rpath, release) <- runHap $ do
            release <- Hap.pushRelease $ mkTask deployPath repoPath
            (,) <$> Hap.releasePath deployPath release (Just workingDir)
                <*> pure release

          rel <- parseRelDir $ renderRelease release
          rpath `shouldBe` deployPath </> releaseDir </> rel </> workingDir

    describe "pushRelease" $ do
      it "sets up repo all right in Zsh" $ \(deployPath, repoPath) ->
        runHapWithShell Zsh $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          rpath <- Hap.releasePath deployPath release Nothing
        -- let's check that the dir exists and contains the right files
          (liftIO . readFile . fromAbsFile) (rpath </> $(mkRelFile "foo.txt")) `shouldReturn`
            "Foo!\n"
      it "sets up repo all right" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          rpath <- Hap.releasePath deployPath release Nothing
        -- let's check that the dir exists and contains the right files
          (liftIO . readFile . fromAbsFile) (rpath </> $(mkRelFile "foo.txt")) `shouldReturn`
            "Foo!\n"
      it "deploys properly a branch other than master" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTaskWithCustomRevision deployPath repoPath testBranchName
          release <- Hap.pushRelease task
          rpath <- Hap.releasePath deployPath release Nothing
        -- let's check that the dir exists and contains the right files
          (liftIO . readFile . fromAbsFile) (rpath </> $(mkRelFile "bar.txt")) `shouldReturn`
            "Bar!\n"
        -- This fails if the opened branch is not testBranchName
          justExec
            rpath
            ("test `git rev-parse --abbrev-ref HEAD` = " ++ testBranchName)
        -- This fails if there are unstaged changes
          justExec rpath "git diff --exit-code"
      it "updates the origin url when it's changed" $ \(deployPath, repoPath) ->
         withSystemTempDir "hap-test-repotwo" $ \repoPathTwo -> do
          runHap $ do
            let task1 = mkTask deployPath repoPath
                task2 = mkTask deployPath repoPathTwo
                repoConfigFile = deployPath </> $(mkRelDir "repo") </> $(mkRelFile "config")
            liftIO $ populateTestRepo repoPathTwo
            void $ Hap.pushRelease task1
            void $ Hap.pushRelease task2

            repoFile <- (liftIO . readFile . fromAbsFile) repoConfigFile
            repoFile `shouldContain` "hap-test-repotwo"

    describe "createHapistranoDeployState" $ do
      it ("creates the " <> deployStateFilename <> " file correctly") $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          parseStatePath <- parseRelFile deployStateFilename
          actualReleasePath <- releasePath deployPath release Nothing
          let stateFilePath = actualReleasePath </> parseStatePath
          Hap.createHapistranoDeployState deployPath release Success
          Path.IO.doesFileExist stateFilePath `shouldReturn`
            True
      it "when created in a successful deploy, the contents are \"Success\"" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          Hap.createHapistranoDeployState deployPath release Success
          Hap.deployState deployPath Nothing release `shouldReturn`
            Success

    describe "activateRelease" $
      it "creates the ‘current’ symlink correctly" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          Hap.activateRelease currentSystem deployPath release
          rpath <- Hap.releasePath deployPath release Nothing
          let rc :: Hap.Readlink Dir
              rc =
                Hap.Readlink currentSystem (Hap.currentSymlinkPath deployPath)
          Hap.exec rc (Just release) `shouldReturn` rpath
          Path.IO.doesFileExist (Hap.tempSymlinkPath deployPath) `shouldReturn` False
    describe "playScriptLocally (successful run)" $
      it "check that local scripts are run and deployment is successful" $ \(deployPath, repoPath) ->
        runHap $ do
          let localCommands = mapMaybe Hap.mkGenericCommand ["pwd", "ls"]
              task = mkTask deployPath repoPath
          Hap.playScriptLocally localCommands
          release <- Hap.pushRelease task
          parseStatePath <- parseRelFile deployStateFilename
          actualReleasePath <- releasePath deployPath release Nothing
          let stateFilePath = actualReleasePath </> parseStatePath
          Hap.createHapistranoDeployState deployPath release Success
          Path.IO.doesFileExist stateFilePath `shouldReturn`
            True
    describe "playScriptLocally (error exit)" $
      it "check that deployment isn't done" $ \(deployPath, repoPath) ->
        (runHap $ do
           let localCommands =
                 mapMaybe Hap.mkGenericCommand ["pwd", "ls", "false"]
               task = mkTask deployPath repoPath
           Hap.playScriptLocally localCommands
           release <- Hap.pushRelease task
           Hap.createHapistranoDeployState deployPath release Success) `shouldThrow`
        anyException
    describe "rollback" $ do
      it "resets the ‘current’ symlink correctly" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTask deployPath repoPath
          rs <- replicateM 5 (Hap.pushRelease task)
          Hap.rollback currentSystem deployPath 2
          rpath <- Hap.releasePath deployPath (rs !! 2) Nothing
          let rc :: Hap.Readlink Dir
              rc =
                Hap.Readlink currentSystem (Hap.currentSymlinkPath deployPath)
          Hap.exec rc Nothing `shouldReturn` rpath
          Path.IO.doesFileExist (Hap.tempSymlinkPath deployPath) `shouldReturn` False
    describe "dropOldReleases" $ do
      it "works" $ \(deployPath, repoPath) ->
        runHap $ do
          rs <-
            replicateM 7 $ do
              r <- Hap.pushRelease (mkTask deployPath repoPath)
              Hap.createHapistranoDeployState deployPath r Success
              return r
          Hap.dropOldReleases deployPath 5 False
          -- two oldest releases should not survive:
          forM_ (take 2 rs) $ \r ->
            (Hap.releasePath deployPath r Nothing >>= doesDirExist) `shouldReturn` False
          -- 5 most recent releases should stay alive:
          forM_ (drop 2 rs) $ \r ->
            (Hap.releasePath deployPath r Nothing >>= doesDirExist) `shouldReturn` True
      context "when the --keep-one-failed flag is active" $
        it "should delete failed releases other than the most recent" $ \(deployPath, repoPath) ->
          let successfulRelease = mkReleaseWithState deployPath repoPath Success
              failedRelease = mkReleaseWithState deployPath repoPath Fail in
          runHap $ do
            rs <- sequence [successfulRelease, successfulRelease, failedRelease, failedRelease, failedRelease]
            Hap.dropOldReleases deployPath 5 True
            -- The two successful releases should survive
            forM_ (take 2 rs) $ \r ->
              (Hap.releasePath deployPath r Nothing >>= doesDirExist) `shouldReturn` True
            -- The latest failed release should survive:
            forM_ (drop 4 rs) $ \r ->
              (Hap.releasePath deployPath r Nothing >>= doesDirExist) `shouldReturn` True
            -- The two older failed releases should not survive:
            forM_ (take 2 . drop 2 $ rs) $ \r ->
              (Hap.releasePath deployPath r Nothing >>= doesDirExist) `shouldReturn` False
    describe "linkToShared" $ do
      context "when the deploy_path/shared directory doesn't exist" $
        it "should create the link anyway" $ \(deployPath, repoPath) ->
          runHap $ do
            let task = mkTask deployPath repoPath
                sharedDir = Hap.sharedPath deployPath
            release <- Hap.pushRelease task
            rpath <- Hap.releasePath deployPath release Nothing
            Hap.exec (Hap.Rm sharedDir) (Just release)
            Hap.linkToShared currentSystem rpath deployPath "thing" (Just release) `shouldReturn`
              ()
      context "when the file/directory to link exists in the repository" $
        it "should throw an error" $ \(deployPath, repoPath) ->
          runHap
            (do let task = mkTask deployPath repoPath
                release <- Hap.pushRelease task
                rpath <- Hap.releasePath deployPath release Nothing
                Hap.linkToShared currentSystem rpath deployPath "foo.txt" $ Just release) `shouldThrow`
          anyException
      context "when it attempts to link a file" $ do
        context "when the file is not at the root of the shared directory" $
          it "should throw an error" $ \(deployPath, repoPath) ->
            runHap
              (do let task = mkTask deployPath repoPath
                      sharedDir = Hap.sharedPath deployPath
                  release <- Hap.pushRelease task
                  rpath <- Hap.releasePath deployPath release Nothing
                  justExec sharedDir "mkdir foo/"
                  justExec sharedDir "echo 'Bar!' > foo/bar.txt"
                  Hap.linkToShared currentSystem rpath deployPath "foo/bar.txt" $ Just release) `shouldThrow`
            anyException
        context "when the file is at the root of the shared directory" $
          it "should link the file successfully" $ \(deployPath, repoPath) ->
            runHap $ do
              let task = mkTask deployPath repoPath
                  sharedDir = Hap.sharedPath deployPath
              release <- Hap.pushRelease task
              rpath <- Hap.releasePath deployPath release Nothing
              justExec sharedDir "echo 'Bar!' > bar.txt"
              Hap.linkToShared currentSystem rpath deployPath "bar.txt" (Just release)
              (liftIO . readFile . fromAbsFile)
                (rpath </> $(mkRelFile "bar.txt")) `shouldReturn`
                "Bar!\n"
      context "when it attemps to link a directory" $ do
        context "when the directory ends in '/'" $
          it "should throw an error" $ \(deployPath, repoPath) ->
            runHap
              (do let task = mkTask deployPath repoPath
                      sharedDir = Hap.sharedPath deployPath
                  release <- Hap.pushRelease task
                  rpath <- Hap.releasePath deployPath release Nothing
                  justExec sharedDir "mkdir foo/"
                  justExec sharedDir "echo 'Bar!' > foo/bar.txt"
                  justExec sharedDir "echo 'Baz!' > foo/baz.txt"
                  Hap.linkToShared currentSystem rpath deployPath "foo/" $ Just release) `shouldThrow`
            anyException
        it "should link the file successfully" $ \(deployPath, repoPath) ->
          runHap $ do
            let task = mkTask deployPath repoPath
                sharedDir = Hap.sharedPath deployPath
            release <- Hap.pushRelease task
            rpath <- Hap.releasePath deployPath release Nothing
            justExec sharedDir "mkdir foo/"
            justExec sharedDir "echo 'Bar!' > foo/bar.txt"
            justExec sharedDir "echo 'Baz!' > foo/baz.txt"
            Hap.linkToShared currentSystem rpath deployPath "foo" (Just release)
            files <-
              (liftIO . listDirectory . fromAbsDir)
                (rpath </> $(mkRelDir "foo"))
            liftIO $ files `shouldMatchList` ["baz.txt", "bar.txt"]

----------------------------------------------------------------------------
-- Helpers
infix 1 `shouldBe`, `shouldReturn`

-- | Lifted 'Hspec.shouldBe'.
shouldBe :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe x y = liftIO (x `Hspec.shouldBe` y)

-- | Lifted 'Hspec.shouldContain'.
shouldContain :: (MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldContain x y = liftIO (x `Hspec.shouldContain` y)

-- | Lifted 'Hspec.shouldReturn'.
shouldReturn :: (MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn m y = m >>= (`shouldBe` y)

-- | The sandbox prepares the environment for an independent round of
-- testing. It provides two paths: deploy path and path where git repo is
-- located.
withSandbox :: ActionWith (Path Abs Dir, Path Abs Dir) -> IO ()
withSandbox action =
  withSystemTempDir "hap-test" $ \dir -> do
    let dpath = dir </> $(mkRelDir "deploy")
        rpath = dir </> $(mkRelDir "repo")
    ensureDir dpath
    ensureDir rpath
    populateTestRepo rpath
    action (dpath, rpath)

-- | Given path where to put the repo, generate it for testing.
populateTestRepo :: Path Abs Dir -> IO ()
populateTestRepo path =
  runHap $ do
    justExec path "git init"
    justExec path "git config --local --replace-all push.default simple"
    justExec path "git config --local --replace-all user.email   hap@hap"
    justExec path "git config --local --replace-all user.name    Hap"
    justExec path "echo 'Foo!' > foo.txt"
    justExec path "git add -A"
    justExec path "git commit -m 'Initial commit'"
  -- Add dummy content to a branch that is not master
    justExec path ("git checkout -b " ++ testBranchName)
    justExec path "echo 'Bar!' > bar.txt"
    justExec path "git add bar.txt"
    justExec path "git commit -m 'Added more bars to another branch'"
    justExec path "git checkout master"

-- | Execute arbitrary commands in the specified directory.
justExec :: Path Abs Dir -> String -> Hapistrano ()
justExec path cmd' =
  case Hap.mkGenericCommand cmd' of
    Nothing -> Hap.failWith 1 (Just $ "Failed to parse the command: " ++ cmd') Nothing
    Just cmd -> Hap.exec (Hap.Cd path cmd) Nothing

-- | Run 'Hapistrano' monad locally.
runHap :: Hapistrano a -> IO a
runHap = runHapWithShell Bash

-- | Run 'Hapistrano' monad setting a particular shell.
runHapWithShell :: Shell -> Hapistrano a -> IO a
runHapWithShell shell m = do
  let printFnc dest str =
        case dest of
          StdoutDest -> putStr str
          StderrDest -> hPutStr stderr str
  r <- Hap.runHapistrano Nothing shell printFnc m
  case r of
    Left n -> do
      expectationFailure ("Failed with status code: " ++ show n)
      return undefined
      -- ↑ because expectationFailure from Hspec has wrong type :-(
    Right x -> return x

-- | Make a 'Task' given deploy path and path to the repo.
mkTask :: Path Abs Dir -> Path Abs Dir -> Task
mkTask deployPath repoPath =
  mkTaskWithCustomRevision deployPath repoPath "master"

mkTaskWithCustomRevision :: Path Abs Dir -> Path Abs Dir -> String -> Task
mkTaskWithCustomRevision deployPath repoPath revision =
  Task
    { taskDeployPath = deployPath
    , taskSource =
        GitRepository
          { gitRepositoryURL = fromAbsDir repoPath
          , gitRepositoryRevision = revision
          }
    , taskReleaseFormat = ReleaseLong
    }

-- | Creates a release tagged with 'Success' or 'Fail'

mkReleaseWithState :: Path Abs Dir -> Path Abs Dir -> DeployState -> Hapistrano Release
mkReleaseWithState deployPath repoPath state = do
  r <- Hap.pushRelease (mkTask deployPath repoPath)
  Hap.createHapistranoDeployState deployPath r state
  return r

currentSystem :: TargetSystem
currentSystem =
  if os == "linux"
    then GNULinux
    else BSD

arbitraryReleaseFormat :: Gen ReleaseFormat
arbitraryReleaseFormat = elements [ReleaseShort, ReleaseLong]

arbitraryKeepReleases :: Gen Natural
arbitraryKeepReleases = fromInteger . getPositive <$> arbitrary
