-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Literal
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Java literals. Used in both parsing and in the Java AST
--
----------------------------------------------------------------------------


module Language.Java.Literal where

import Language.Java.Char

import Data.Int

data Literal
  = IntL Int32
  | LongL Int64
  | FloatL Float
  | DoubleL Double
  | BooleanL Bool
  | CharL JChar   -- NB: Haskell's Char has a greater range than Java's char
  | StringL JString
  | NullL
  deriving (Show)
