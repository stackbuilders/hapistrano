{-# LANGUAGE TemplateHaskell #-}

module System.HapistranoSpec
  ( spec )
where

import Control.Monad
import Control.Monad.Reader
import Path
import Path.IO
import Data.Maybe (catMaybes)
import System.Hapistrano.Types
import System.IO
import Test.Hspec hiding (shouldBe, shouldReturn)
import qualified System.Hapistrano as Hap
import qualified System.Hapistrano.Commands as Hap
import qualified System.Hapistrano.Core as Hap
import qualified Test.Hspec as Hspec

spec :: Spec
spec = do
  describe "readScript" $
    it "preforms all the necessary normalizations correctly" $ do
      spath <- makeAbsolute $(mkRelFile "script/clean-build.sh")
      (fmap Hap.unGenericCommand <$> Hap.readScript spath)
        `Hspec.shouldReturn`
        [ "export PATH=~/.cabal/bin:/usr/local/bin:$PATH"
        , "cabal sandbox delete"
        , "cabal sandbox init"
        , "cabal clean"
        , "cabal update"
        , "cabal install --only-dependencies -j"
        , "cabal build -j" ]

  around withSandbox $ do
    describe "pushRelease" $
      it "sets up repo all right" $ \(deployPath, repoPath) -> runHap $ do
        let task = mkTask deployPath repoPath
        release <- Hap.pushRelease task
        rpath   <- Hap.releasePath deployPath release
        -- let's check that the dir exists and contains the right files
        (liftIO . readFile . fromAbsFile) (rpath </> $(mkRelFile "foo.txt"))
          `shouldReturn` "Foo!\n"

    describe "registerReleaseAsComplete" $
      it "creates the token all right" $ \(deployPath, repoPath) -> runHap $ do
        let task = mkTask deployPath repoPath
        release <- Hap.pushRelease task
        Hap.registerReleaseAsComplete deployPath release
        (Hap.ctokenPath deployPath release >>= doesFileExist) `shouldReturn` True

    describe "activateRelease" $
      it "creates the ‘current’ symlink correctly" $ \(deployPath, repoPath) -> runHap $ do
        let task = mkTask deployPath repoPath
        release <- Hap.pushRelease task
        Hap.activateRelease deployPath release
        rpath <- Hap.releasePath deployPath release
        let rc :: Hap.Readlink Dir
            rc = Hap.Readlink (Hap.currentSymlinkPath deployPath)
        Hap.exec rc `shouldReturn` rpath
        doesFileExist (Hap.tempSymlinkPath deployPath) `shouldReturn` False
        
    describe "playScriptLocally (successful run)" $
       it "check that local scripts are run and deployment is successful" $ \(deployPath, repoPath) -> runHap $ do
        let localCommands = catMaybes $ map Hap.mkGenericCommand ["pwd", "ls"]
            task = mkTask deployPath repoPath
        Hap.playScriptLocally localCommands
        release <- Hap.pushRelease task
        Hap.registerReleaseAsComplete deployPath release
        (Hap.ctokenPath deployPath release >>= doesFileExist) `shouldReturn` True

    describe "playScriptLocally (error exit)" $
       it "check that deployment isn't done" $ \(deployPath, repoPath) -> (runHap $ do
        let localCommands = catMaybes $ map Hap.mkGenericCommand ["pwd", "ls", "false"]
            task = mkTask deployPath repoPath
        Hap.playScriptLocally localCommands
        release <- Hap.pushRelease task
        Hap.registerReleaseAsComplete deployPath release) `shouldThrow` anyException

    describe "rollback" $ do
      context "without completion tokens" $
        it "resets the ‘current’ symlink correctly" $ \(deployPath, repoPath) -> runHap $ do
          let task = mkTask deployPath repoPath
          rs <- replicateM 5 (Hap.pushRelease task)
          Hap.rollback deployPath 2
          rpath <- Hap.releasePath deployPath (rs !! 2)
          let rc :: Hap.Readlink Dir
              rc = Hap.Readlink (Hap.currentSymlinkPath deployPath)
          Hap.exec rc `shouldReturn` rpath
          doesFileExist (Hap.tempSymlinkPath deployPath) `shouldReturn` False
      context "with completion tokens" $
        it "resets the ‘current’ symlink correctly" $ \(deployPath, repoPath) -> runHap $ do
          let task = mkTask deployPath repoPath
          rs <- replicateM 5 (Hap.pushRelease task)
          forM_ (take 3 rs) (Hap.registerReleaseAsComplete deployPath)
          Hap.rollback deployPath 2
          rpath <- Hap.releasePath deployPath (rs !! 0)
          let rc :: Hap.Readlink Dir
              rc = Hap.Readlink (Hap.currentSymlinkPath deployPath)
          Hap.exec rc `shouldReturn` rpath
          doesFileExist (Hap.tempSymlinkPath deployPath) `shouldReturn` False

    describe "dropOldReleases" $
      it "works" $ \(deployPath, repoPath) -> runHap $ do
        rs <- replicateM 7 $ do
          r <- Hap.pushRelease (mkTask deployPath repoPath)
          Hap.registerReleaseAsComplete deployPath r
          return r
        Hap.dropOldReleases deployPath 5
        -- two oldest releases should not survive:
        forM_ (take 2 rs) $ \r ->
          (Hap.releasePath deployPath r >>= doesDirExist)
            `shouldReturn` False
        -- 5 most recent releases should stay alive:
        forM_ (drop 2 rs) $ \r ->
          (Hap.releasePath deployPath r >>= doesDirExist)
            `shouldReturn` True
        -- two oldest completion tokens should not survive:
        forM_ (take 2 rs) $ \r ->
          (Hap.ctokenPath deployPath r >>= doesFileExist)
            `shouldReturn` False
        -- 5 most recent completion tokens should stay alive:
        forM_ (drop 2 rs) $ \r ->
          (Hap.ctokenPath deployPath r >>= doesFileExist)
            `shouldReturn` True

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
withSandbox action = withSystemTempDir "hap-test" $ \dir -> do
  let dpath = dir </> $(mkRelDir "deploy")
      rpath = dir </> $(mkRelDir "repo")
  ensureDir dpath
  ensureDir rpath
  populateTestRepo rpath
  action (dpath, rpath)

-- | Given path where to put the repo, generate it for testing.

populateTestRepo :: Path Abs Dir -> IO ()
populateTestRepo path = runHap $ do
  justExec path "git init"
  justExec path "git config --local --replace-all push.default simple"
  justExec path "git config --local --replace-all user.email   hap@hap"
  justExec path "git config --local --replace-all user.name    Hap"
  justExec path "echo 'Foo!' > foo.txt"
  justExec path "git add -A"
  justExec path "git commit -m 'Initial commit'"

-- | Execute arbitrary commands in the specified directory.

justExec :: Path Abs Dir -> String -> Hapistrano ()
justExec path cmd' =
  case Hap.mkGenericCommand cmd' of
    Nothing -> Hap.failWith 1 (Just $ "Failed to parse the command: " ++ cmd')
    Just cmd -> Hap.exec (Hap.Cd path cmd)

-- | Run 'Hapistrano' monad locally.

runHap :: Hapistrano a -> IO a
runHap m = do
  let printFnc dest str =
        case dest of
          StdoutDest -> putStr str
          StderrDest -> hPutStr stderr str
  r <- Hap.runHapistrano Nothing printFnc m
  case r of
    Left n -> do
      expectationFailure ("Failed with status code: " ++ show n)
      return undefined
      -- ↑ because expectationFailure from Hspec has wrong type :-(
    Right x -> return x

-- | Make a 'Task' given deploy path and path to the repo.

mkTask :: Path Abs Dir -> Path Abs Dir -> Task
mkTask deployPath repoPath = Task
  { taskDeployPath    = deployPath
  , taskRepository    = fromAbsDir repoPath
  , taskRevision      = "master"
  , taskReleaseFormat = ReleaseLong }
