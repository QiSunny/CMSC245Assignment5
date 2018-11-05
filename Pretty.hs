-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Pretty
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines the class Pretty, used for pretty-printing internal AST.
-- This differs from Show in that Pretty output is meant for users, not
-- implementors. ('Show' shows the internal syntax.)
--
-- Note: This is not a full pretty-printer -- just pretty strings, really.
--
----------------------------------------------------------------------------

module Language.Java.Pretty where

-- | Members of this class can be rendered in a way suitable for users.
class Pretty a where
  -- | Convert an entity to a user-appropriate string
  ppr :: a -> String
