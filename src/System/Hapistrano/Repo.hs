module System.Hapistrano.Repo
  ( createOrUpdateRepo
  ) where

import           Development.Shake

createOrUpdateRepo :: String -> FilePath -> Action ()
createOrUpdateRepo repository repoPath = do
  exists <- doesDirectoryExist repoPath
  if exists
    then updateRepo repoPath
    else createRepo repository repoPath

createRepo :: String -> FilePath -> Action ()
createRepo repository repoPath =
  cmd "git clone --bare" [repository, repoPath]

updateRepo :: String -> Action ()
updateRepo repoPath =
  cmd [Cwd repoPath] "git fetch origin +refs/heads/*:refs/heads/*"
