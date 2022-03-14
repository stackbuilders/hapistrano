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
    exec (BasicWrite root "<!DOCTYPE html><html><head><title>Maintenance</title><style type=\"text/css\">body {width: 400px;margin: 100px auto;font: 300 120% \"OpenSans\", \"Helvetica Neue\", \"Helvetica\", Arial, Verdana, sans-serif;}h1 {font-weight: 300;}</style></head><body><h1>Maintenance</h1><p>The system is down for maintenance</p><p>It'll be back shortly</p></body></html>") Nothing

  