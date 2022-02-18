{-# LANGUAGE TemplateHaskell #-}

module System.Utils (
  runHap,
  withSandbox
) where

import Path (mkAbsDir, mkRelDir, Rel, Abs, Dir, Path, (</>))
import System.Hapistrano.Types
import Test.Hspec
import System.IO
import Path.IO
import qualified System.Hapistrano as Hap
import qualified System.Hapistrano.Commands as Hap
import qualified System.Hapistrano.Core as Hap

-- | Run 'Hapistrano' monad locally.
runHap :: Hapistrano a -> IO a
runHap = runHapWithShell Bash

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

-- | Execute arbitrary commands in the specified directory.
justExec :: Path Abs Dir -> String -> Hapistrano ()
justExec path cmd' =
  case Hap.mkGenericCommand cmd' of
    Nothing -> Hap.failWith 1 (Just $ "Failed to parse the command: " ++ cmd') Nothing
    Just cmd -> Hap.exec (Hap.Cd path cmd) Nothing


testBranchName :: String
testBranchName = "another_branch"
