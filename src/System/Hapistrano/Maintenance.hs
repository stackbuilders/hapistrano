{-# LANGUAGE TemplateHaskell #-}
module System.Hapistrano.Maintenance (
  writeMaintenanceFile,
  deleteMaintenanceFile
) where

import           Path                       (Abs, Dir, File, Path, Rel,
                                             mkAbsDir, mkRelFile, parseRelDir,
                                             (</>))
import           System.Hapistrano.Commands
import           System.Hapistrano.Core
import           System.Hapistrano.Types

-- | It writes an HTML page in the given directory with a given name
writeMaintenanceFile :: Path Abs Dir -> Path Rel Dir -> Path Rel File -> Hapistrano()
writeMaintenanceFile deployPath relDir fileName =
  let foo = deployPath </> relDir
      fullpath = relDir </> fileName
      root = deployPath </> fullpath
  in do
    exec(MkDir foo) Nothing
    exec(Touch root) Nothing
    exec (BasicWrite root maintenancePageContent) Nothing

-- | It deletes the file in the given directory with the given name
deleteMaintenanceFile :: Path Abs Dir -> Path Rel Dir -> Path Rel File -> Hapistrano()
deleteMaintenanceFile deployPath relDir fileName =
  let fullpath = relDir </> fileName
      root = deployPath </> fullpath
  in
    exec(Rm root) Nothing

maintenancePageContent :: String
maintenancePageContent = "
<!DOCTYPE html>
<html>

<head>
  <title>Maintenance</title>
  <style type=\"text/css\">
    body {
      width: 400px;
      margin: 100px auto;
      font: 300 120% \"OpenSans\", \"Helvetica Neue\", \"Helvetica\", Arial, Verdana, sans-serif;
    }

    h1 {
      font-weight: 300;
    }
  </style>
</head>

<body>
  <h1>Maintenance</h1>
  <p>The system is down for maintenance</p>
  <p>It'll be back shortly</p>
</body>

</html>"
