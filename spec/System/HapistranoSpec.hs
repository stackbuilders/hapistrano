{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module System.HapistranoSpec
  ( spec
  ) where

import Control.Monad
import Control.Monad.Reader
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Numeric.Natural
import Path
import Path.IO
import System.Directory (getCurrentDirectory, listDirectory)
import qualified System.Hapistrano as Hap
import qualified System.Hapistrano.Commands as Hap
import qualified System.Hapistrano.Core as Hap
import System.Hapistrano.Types
import System.IO
import System.IO.Silently (capture_)
import System.Info (os)
import Test.Hspec hiding (shouldBe, shouldReturn)
import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

testBranchName :: String
testBranchName = "another_branch"

spec :: Spec
spec = do
  describe "execWithInheritStdout" $
    context "given a command that prints to stdout" $
    it "redirects commands' output to stdout first" $
    let (Just commandTest) =
          Hap.mkGenericCommand
            "echo \"hapistrano\"; sleep 2; echo \"onartsipah\""
        commandExecution = Hap.execWithInheritStdout commandTest
        expectedOutput = "hapistrano\nonartsipah"
     in do actualOutput <- capture_ (runHap commandExecution)
           expectedOutput `Hspec.shouldSatisfy` (`isPrefixOf` actualOutput)
  describe "readScript" $
    it "performs all the necessary normalizations correctly" $ do
#if MIN_VERSION_path_io(1,6,0)
      spath <- do
        currentDirectory <- getCurrentDirectory
        parseAbsFile (currentDirectory ++ "script/clean-build.sh")
#else
      spath <- makeAbsolute $(mkRelFile "script/clean-build.sh")
#endif
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
    describe "pushRelease" $ do
      it "sets up repo all right in Zsh" $ \(deployPath, repoPath) ->
        runHapWithShell Zsh $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          rpath <- Hap.releasePath deployPath release
        -- let's check that the dir exists and contains the right files
          (liftIO . readFile . fromAbsFile) (rpath </> $(mkRelFile "foo.txt")) `shouldReturn`
            "Foo!\n"
      it "sets up repo all right" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          rpath <- Hap.releasePath deployPath release
        -- let's check that the dir exists and contains the right files
          (liftIO . readFile . fromAbsFile) (rpath </> $(mkRelFile "foo.txt")) `shouldReturn`
            "Foo!\n"
      it "deploys properly a branch other than master" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTaskWithCustomRevision deployPath repoPath testBranchName
          release <- Hap.pushRelease task
          rpath <- Hap.releasePath deployPath release
        -- let's check that the dir exists and contains the right files
          (liftIO . readFile . fromAbsFile) (rpath </> $(mkRelFile "bar.txt")) `shouldReturn`
            "Bar!\n"
        -- This fails if the opened branch is not testBranchName
          justExec
            rpath
            ("test `git rev-parse --abbrev-ref HEAD` = " ++ testBranchName)
        -- This fails if there are unstaged changes
          justExec rpath "git diff --exit-code"
    describe "registerReleaseAsComplete" $
      it "creates the token all right" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          Hap.registerReleaseAsComplete deployPath release
          (Hap.ctokenPath deployPath release >>= doesFileExist) `shouldReturn`
            True
    describe "activateRelease" $
      it "creates the ‘current’ symlink correctly" $ \(deployPath, repoPath) ->
        runHap $ do
          let task = mkTask deployPath repoPath
          release <- Hap.pushRelease task
          Hap.activateRelease currentSystem deployPath release
          rpath <- Hap.releasePath deployPath release
          let rc :: Hap.Readlink Dir
              rc =
                Hap.Readlink currentSystem (Hap.currentSymlinkPath deployPath)
          Hap.exec rc `shouldReturn` rpath
          doesFileExist (Hap.tempSymlinkPath deployPath) `shouldReturn` False
    describe "playScriptLocally (successful run)" $
      it "check that local scripts are run and deployment is successful" $ \(deployPath, repoPath) ->
        runHap $ do
          let localCommands = mapMaybe Hap.mkGenericCommand ["pwd", "ls"]
              task = mkTask deployPath repoPath
          Hap.playScriptLocally localCommands
          release <- Hap.pushRelease task
          Hap.registerReleaseAsComplete deployPath release
          (Hap.ctokenPath deployPath release >>= doesFileExist) `shouldReturn`
            True
    describe "playScriptLocally (error exit)" $
      it "check that deployment isn't done" $ \(deployPath, repoPath) ->
        (runHap $ do
           let localCommands =
                 mapMaybe Hap.mkGenericCommand ["pwd", "ls", "false"]
               task = mkTask deployPath repoPath
           Hap.playScriptLocally localCommands
           release <- Hap.pushRelease task
           Hap.registerReleaseAsComplete deployPath release) `shouldThrow`
        anyException
    describe "rollback" $ do
      context "without completion tokens" $
        it "resets the ‘current’ symlink correctly" $ \(deployPath, repoPath) ->
          runHap $ do
            let task = mkTask deployPath repoPath
            rs <- replicateM 5 (Hap.pushRelease task)
            Hap.rollback currentSystem deployPath 2
            rpath <- Hap.releasePath deployPath (rs !! 2)
            let rc :: Hap.Readlink Dir
                rc =
                  Hap.Readlink currentSystem (Hap.currentSymlinkPath deployPath)
            Hap.exec rc `shouldReturn` rpath
            doesFileExist (Hap.tempSymlinkPath deployPath) `shouldReturn` False
      context "with completion tokens" $
        it "resets the ‘current’ symlink correctly" $ \(deployPath, repoPath) ->
          runHap $ do
            let task = mkTask deployPath repoPath
            rs <- replicateM 5 (Hap.pushRelease task)
            forM_ (take 3 rs) (Hap.registerReleaseAsComplete deployPath)
            Hap.rollback currentSystem deployPath 2
            rpath <- Hap.releasePath deployPath (rs !! 0)
            let rc :: Hap.Readlink Dir
                rc =
                  Hap.Readlink currentSystem (Hap.currentSymlinkPath deployPath)
            Hap.exec rc `shouldReturn` rpath
            doesFileExist (Hap.tempSymlinkPath deployPath) `shouldReturn` False
    describe "dropOldReleases" $
      it "works" $ \(deployPath, repoPath) ->
        runHap $ do
          rs <-
            replicateM 7 $ do
              r <- Hap.pushRelease (mkTask deployPath repoPath)
              Hap.registerReleaseAsComplete deployPath r
              return r
          Hap.dropOldReleases deployPath 5
        -- two oldest releases should not survive:
          forM_ (take 2 rs) $ \r ->
            (Hap.releasePath deployPath r >>= doesDirExist) `shouldReturn` False
        -- 5 most recent releases should stay alive:
          forM_ (drop 2 rs) $ \r ->
            (Hap.releasePath deployPath r >>= doesDirExist) `shouldReturn` True
        -- two oldest completion tokens should not survive:
          forM_ (take 2 rs) $ \r ->
            (Hap.ctokenPath deployPath r >>= doesFileExist) `shouldReturn` False
        -- 5 most recent completion tokens should stay alive:
          forM_ (drop 2 rs) $ \r ->
            (Hap.ctokenPath deployPath r >>= doesFileExist) `shouldReturn` True
    describe "linkToShared" $ do
      context "when the deploy_path/shared directory doesn't exist" $
        it "should create the link anyway" $ \(deployPath, repoPath) ->
          runHap $ do
            let task = mkTask deployPath repoPath
                sharedDir = Hap.sharedPath deployPath
            release <- Hap.pushRelease task
            rpath <- Hap.releasePath deployPath release
            Hap.exec $ Hap.Rm sharedDir
            Hap.linkToShared currentSystem rpath deployPath "thing" `shouldReturn`
              ()
      context "when the file/directory to link exists in the respository" $
        it "should throw an error" $ \(deployPath, repoPath) ->
          runHap
            (do let task = mkTask deployPath repoPath
                release <- Hap.pushRelease task
                rpath <- Hap.releasePath deployPath release
                Hap.linkToShared currentSystem rpath deployPath "foo.txt") `shouldThrow`
          anyException
      context "when it attemps to link a file" $ do
        context "when the file is not at the root of the shared directory" $
          it "should throw an error" $ \(deployPath, repoPath) ->
            runHap
              (do let task = mkTask deployPath repoPath
                      sharedDir = Hap.sharedPath deployPath
                  release <- Hap.pushRelease task
                  rpath <- Hap.releasePath deployPath release
                  justExec sharedDir "mkdir foo/"
                  justExec sharedDir "echo 'Bar!' > foo/bar.txt"
                  Hap.linkToShared currentSystem rpath deployPath "foo/bar.txt") `shouldThrow`
            anyException
        context "when the file is at the root of the shared directory" $
          it "should link the file successfully" $ \(deployPath, repoPath) ->
            runHap $ do
              let task = mkTask deployPath repoPath
                  sharedDir = Hap.sharedPath deployPath
              release <- Hap.pushRelease task
              rpath <- Hap.releasePath deployPath release
              justExec sharedDir "echo 'Bar!' > bar.txt"
              Hap.linkToShared currentSystem rpath deployPath "bar.txt"
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
                  rpath <- Hap.releasePath deployPath release
                  justExec sharedDir "mkdir foo/"
                  justExec sharedDir "echo 'Bar!' > foo/bar.txt"
                  justExec sharedDir "echo 'Baz!' > foo/baz.txt"
                  Hap.linkToShared currentSystem rpath deployPath "foo/") `shouldThrow`
            anyException
        it "should link the file successfully" $ \(deployPath, repoPath) ->
          runHap $ do
            let task = mkTask deployPath repoPath
                sharedDir = Hap.sharedPath deployPath
            release <- Hap.pushRelease task
            rpath <- Hap.releasePath deployPath release
            justExec sharedDir "mkdir foo/"
            justExec sharedDir "echo 'Bar!' > foo/bar.txt"
            justExec sharedDir "echo 'Baz!' > foo/baz.txt"
            Hap.linkToShared currentSystem rpath deployPath "foo"
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
    Nothing -> Hap.failWith 1 (Just $ "Failed to parse the command: " ++ cmd')
    Just cmd -> Hap.exec (Hap.Cd path cmd)

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

currentSystem :: TargetSystem
currentSystem =
  if os == "linux"
    then GNULinux
    else BSD

arbitraryReleaseFormat :: Gen ReleaseFormat
arbitraryReleaseFormat = elements [ReleaseShort, ReleaseLong]

arbitraryKeepReleases :: Gen Natural
arbitraryKeepReleases = fromInteger . getPositive <$> arbitrary
