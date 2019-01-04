-- |
-- Module      :  System.Hapistrano.Commands
-- Copyright   :  © 2015-2017 Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of type safe shell commands that can be fed into
-- 'System.Hapistrano.Core.runCommand'.

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module System.Hapistrano.Commands
  ( Command (..)
  , Whoami (..)
  , Cd (..)
  , MkDir (..)
  , Rm (..)
  , Mv (..)
  , Ln (..)
  , Ls (..)
  , Readlink (..)
  , Find (..)
  , Touch (..)
  , GitCheckout (..)
  , GitClone (..)
  , GitFetch (..)
  , GitReset (..)
  , GenericCommand
  , mkGenericCommand
  , unGenericCommand
  , readScript )
where

import           Control.Monad.IO.Class
import           Data.Char               (isSpace)
import           Data.List               (dropWhileEnd)
import           Data.Maybe              (catMaybes, fromJust, mapMaybe)
import           Data.Proxy
import           Numeric.Natural
import           Path

import           System.Hapistrano.Types (TargetSystem (..))

----------------------------------------------------------------------------
-- Commands

-- | Class for data types that represent shell commands in typed way.

class Command a where

  -- | Type of result.

  type Result a :: *

  -- | How to render the command before feeding it into shell (possibly via
  -- SSH).

  renderCommand :: a -> String

  -- | How to parse the result from stdout.

  parseResult :: Proxy a -> String -> Result a

-- | Unix @whoami@.

data Whoami = Whoami
  deriving (Show, Eq, Ord)

instance Command Whoami where
  type Result Whoami = String
  renderCommand Whoami = "whoami"
  parseResult Proxy = trim

-- | Specify directory in which to perform another command.

data Cd cmd = Cd (Path Abs Dir) cmd

instance Command cmd => Command (Cd cmd) where
  type Result (Cd cmd) = Result cmd
  renderCommand (Cd path cmd) = "(cd " ++ quoteCmd (fromAbsDir path) ++
    " && " ++ renderCommand cmd ++ ")"
  parseResult Proxy = parseResult (Proxy :: Proxy cmd)

-- | Create a directory. Does not fail if the directory already exists.

data MkDir = MkDir (Path Abs Dir)

instance Command MkDir where
  type Result MkDir = ()
  renderCommand (MkDir path) = formatCmd "mkdir"
    [ Just "-pv"
    , Just (fromAbsDir path) ]
  parseResult Proxy _ = ()

-- | Delete file or directory.

data Rm where
  Rm :: Path Abs t -> Rm

instance Command Rm where
  type Result Rm = ()
  renderCommand (Rm path) = formatCmd "rm"
    [ Just "-rf"
    , Just (toFilePath path) ]
  parseResult Proxy _ = ()

-- | Move or rename files or directories.

data Mv t = Mv TargetSystem (Path Abs t) (Path Abs t)

instance Command (Mv File) where
  type Result (Mv File) = ()
  renderCommand (Mv ts old new) = formatCmd "mv"
    [ Just flags
    , Just (fromAbsFile old)
    , Just (fromAbsFile new) ]
    where flags = if isLinux ts then "-fvT" else "-fv"
  parseResult Proxy _ = ()

instance Command (Mv Dir) where
  type Result (Mv Dir) = ()
  renderCommand (Mv _ old new) = formatCmd "mv"
    [ Just "-fv"
    , Just (fromAbsDir old)
    , Just (fromAbsDir new) ]
  parseResult Proxy _ = ()

-- | Create symlinks.

data Ln where
  Ln :: TargetSystem -> Path Abs t -> Path Abs File -> Ln

instance Command Ln where
  type Result Ln = ()
  renderCommand (Ln ts target linkName) = formatCmd "ln"
    [ Just flags
    , Just (toFilePath target)
    , Just (fromAbsFile linkName) ]
    where flags = if isLinux ts then "-svT" else "-sv"
  parseResult Proxy _ = ()

-- | Read link.

data Readlink t = Readlink TargetSystem (Path Abs File)

instance Command (Readlink File) where
  type Result (Readlink File) = Path Abs File
  renderCommand (Readlink ts path) = formatCmd "readlink"
    [ flags
    , Just (toFilePath path) ]
    where flags = if isLinux ts then Just "-f" else Nothing
  parseResult Proxy = fromJust . parseAbsFile . trim

instance Command (Readlink Dir) where
  type Result (Readlink Dir) = Path Abs Dir
  renderCommand (Readlink ts path) = formatCmd "readlink"
    [ flags
    , Just (toFilePath path) ]
    where flags = if isLinux ts then Just "-f" else Nothing
  parseResult Proxy = fromJust . parseAbsDir . trim

-- | @ls@, so far used only to check existence of directories, so it's not
-- very functional right now.

data Ls = Ls (Path Abs Dir)

instance Command Ls where
  type Result Ls = ()
  renderCommand (Ls path) = formatCmd "ls"
    [ Just (fromAbsDir path) ]
  parseResult Proxy _ = ()

-- | Find (a very limited version).

data Find t = Find Natural (Path Abs Dir)

instance Command (Find Dir) where
  type Result (Find Dir) = [Path Abs Dir]
  renderCommand (Find maxDepth dir) = formatCmd "find"
    [ Just (fromAbsDir dir)
    , Just "-maxdepth"
    , Just (show maxDepth)
    , Just "-type"
    , Just "d" ]
  parseResult Proxy = mapMaybe parseAbsDir . fmap trim . lines

instance Command (Find File) where
  type Result (Find File) = [Path Abs File]
  renderCommand (Find maxDepth dir) = formatCmd "find"
    [ Just (fromAbsDir dir)
    , Just "-maxdepth"
    , Just (show maxDepth)
    , Just "-type"
    , Just "f" ]
  parseResult Proxy = mapMaybe parseAbsFile . fmap trim . lines

-- | @touch@.

data Touch = Touch (Path Abs File)

instance Command Touch where
  type Result Touch = ()
  renderCommand (Touch path) = formatCmd "touch"
    [ Just (fromAbsFile path) ]
  parseResult Proxy _ = ()

-- | Git checkout.

data GitCheckout = GitCheckout String

instance Command GitCheckout where
  type Result GitCheckout = ()
  renderCommand (GitCheckout revision) = formatCmd "git"
    [ Just "checkout"
    , Just revision ]
  parseResult Proxy  _ = ()

-- | Git clone.

data GitClone = GitClone Bool (Either String (Path Abs Dir)) (Path Abs Dir)

instance Command GitClone where
  type Result GitClone = ()
  renderCommand (GitClone bare src dest) = formatCmd "git"
    [ Just "clone"
    , if bare then Just "--bare" else Nothing
    , Just (case src of
       Left repoUrl  -> repoUrl
       Right srcPath -> fromAbsDir srcPath)
    , Just (fromAbsDir dest) ]
  parseResult Proxy _ = ()

-- | Git fetch (simplified).

data GitFetch = GitFetch String

instance Command GitFetch where
  type Result GitFetch = ()
  renderCommand (GitFetch remote) = formatCmd "git"
    [ Just "fetch"
    , Just remote
    , Just "+refs/heads/\\*:refs/heads/\\*" ]
  parseResult Proxy _ = ()

-- | Git reset.

data GitReset = GitReset String

instance Command GitReset where
  type Result GitReset = ()
  renderCommand (GitReset revision) = formatCmd "git"
    [ Just "reset"
    , Just revision ]
  parseResult Proxy _ = ()

-- | Weakly-typed generic command, avoid using it directly.

data GenericCommand = GenericCommand String
  deriving (Show, Eq, Ord)

instance Command GenericCommand where
  type Result GenericCommand = ()
  renderCommand (GenericCommand cmd) = cmd
  parseResult Proxy _ = ()

-- | Smart constructor that allows to create 'GenericCommand's. Just a
-- little bit more safety.

mkGenericCommand :: String -> Maybe GenericCommand
mkGenericCommand str =
  if '\n' `elem` str' || null str'
    then Nothing
    else Just (GenericCommand str')
  where
    str' = trim (takeWhile (/= '#') str)

-- | Get the raw command back from 'GenericCommand'.

unGenericCommand :: GenericCommand -> String
unGenericCommand (GenericCommand x) = x

-- | Read commands from a file.

readScript :: MonadIO m => Path Abs File -> m [GenericCommand]
readScript path = liftIO $ catMaybes . fmap mkGenericCommand . lines
  <$> readFile (fromAbsFile path)

----------------------------------------------------------------------------
-- Helpers

-- | Format a command.

formatCmd :: String -> [Maybe String] -> String
formatCmd cmd args = unwords (quoteCmd <$> (cmd : catMaybes args))

-- | Simple-minded quoter.

quoteCmd :: String -> String
quoteCmd str =
  if any isSpace str
    then "\"" ++ str ++ "\""
    else str

-- | Trim whitespace from beginning and end.

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

isLinux :: TargetSystem -> Bool
isLinux = (== GNULinux)
