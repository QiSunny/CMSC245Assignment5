-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Identifier
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines Java identifiers
--
----------------------------------------------------------------------------

module Language.Java.Identifier where

import Language.Java.Pretty

---------------------------------------------
-- Identifiers

newtype Identifier = Identifier String
  deriving (Eq, Ord, Show)
  -- Needs Eq and Ord instances because it's used as the
  -- key of a map (for object field lookup)

-- The Java grammar has a TypeName separate from an
-- ExpressionName, but there is no way to tell these
-- apart in a LALR parser. (For example, think of the
-- first thing parsed after a "(". Is it a qualified
-- field name? Or the type in a cast? No way to know.
data QualifiedName
  = QualifiedName [Identifier] Identifier
  deriving (Show)

---------------------------------------------
-- Pretty printing

instance Pretty Identifier where
  ppr (Identifier name) = name
