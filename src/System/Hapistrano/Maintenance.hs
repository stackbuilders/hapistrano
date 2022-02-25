{-# LANGUAGE TemplateHaskell   #-}
module System.Hapistrano.Maintenance (
  writeMaintenanceFile
) where

import Path ( mkAbsDir, mkRelFile, fromRelDir, Path, Abs, Rel, Dir, File, (</>))
import System.Hapistrano.Types
import System.Hapistrano.Core
import System.Hapistrano.Commands

writeMaintenanceFile :: Path Abs Dir -> Path Rel Dir -> FilePath -> Hapistrano()
writeMaintenanceFile deployPath relDir fileName =
  let foo = deployPath </> relDir
      fullpath = relDir </> $(mkRelFile "maintenance.html")
      root = deployPath </> fullpath
  in do
    exec(MkDir foo) Nothing
    exec(Touch root) Nothing
    exec (BasicWrite root "<html><body>Maintenance</body></html>") Nothing

  