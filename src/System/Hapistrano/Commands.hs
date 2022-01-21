-- |
-- Module      :  System.Hapistrano.Commands
-- Copyright   :  © 2015-Present Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Juan Paucar <jpaucar@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of type safe shell commands that can be fed into
-- 'System.Hapistrano.Core.runCommand'.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module System.Hapistrano.Commands
  ( Command(..)
  , Whoami(..)
  , Cd(..)
  , MkDir(..)
  , Rm(..)
  , Mv(..)
  , Ln(..)
  , Ls(..)
  , Readlink(..)
  , Find(..)
  , Touch(..)
  , BasicWrite(..)
  , GitCheckout(..)
  , GitClone(..)
  , GitFetch(..)
  , GitReset(..)
  , GenericCommand
  , mkGenericCommand
  , unGenericCommand
  , readScript
  ) where

import System.Hapistrano.Commands.Internal
