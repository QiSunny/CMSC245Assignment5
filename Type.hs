-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Type
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines the type-related Java AST
--
----------------------------------------------------------------------------

module Language.Java.Type where

import Language.Java.Identifier
import Language.Java.Pretty

import Data.List ( intercalate )

------------------------------------------------
-- Types

data Type
  = PrimType PrimitiveType
  | RefType ReferenceType
  deriving (Show)

data PrimitiveType
  = Byte
  | Short
  | Int
  | Long
  | Char
  | Float
  | Double
  | Boolean
  | NullType  -- internal only; cannot be parsed
  deriving (Eq, Show)
  -- During type-checking, we need to compare type for equality

data ReferenceType
  = COIRT COIType
  | ArrayRT ArrayType
  deriving (Show)

data ArrayType
  = PrimArray PrimitiveType Dims
  | COIArray COIType Dims
  deriving (Show)

data TypeArgument
  = ReferenceArgument ReferenceType
  | WildcardArgument Wildcard
  deriving (Show)

data Wildcard
  = Wildcard (Maybe WildcardBounds)
  deriving (Show)

data WildcardBounds
  = ExtendsWB ReferenceType
  | SuperWB ReferenceType
  deriving (Show)

-- This can also be a type variable, if there is no qualifier
-- and no arguments. There is no way to tell the difference
-- between a ClassOrInterfaceType and a type variable until
-- we see what is in scope.
data COIType
  = COIType (Maybe COIType) Identifier [TypeArgument]
  deriving (Show)

type InterfaceType = COIType
type ClassType = COIType
type ExceptionType = COIType

newtype Dims
  = Dims Int   -- INVARIANT: the Int is >= 1
  deriving (Show)

------------------------------------------------
-- Convenience functions

-- | @mkCOIType "String"@ produces the 'COIType' for @String@.
mkCOIType :: String -> COIType
mkCOIType str = COIType Nothing (Identifier str) []

------------------------------------------------
-- Pretty printing

instance Pretty Type where
  ppr (PrimType prim) = ppr prim
  ppr (RefType ref)   = ppr ref

instance Pretty PrimitiveType where
  ppr Byte     = "byte"
  ppr Short    = "short"
  ppr Int      = "int"
  ppr Long     = "long"
  ppr Char     = "char"
  ppr Float    = "float"
  ppr Double   = "double"
  ppr Boolean  = "boolean"
  ppr NullType = "<nulltype>"

instance Pretty ReferenceType where
  ppr (COIRT coi)     = ppr coi
  ppr (ArrayRT a)     = ppr a

instance Pretty COIType where
  ppr (COIType m_parent ident args) = pp_parent ++ ppr ident ++ pp_args
    where
      pp_parent = case m_parent of
        Just parent -> ppr parent ++ "."
        Nothing     -> ""

      pp_args | null args = ""
              | otherwise = intercalate ", " (map ppr args)

instance Pretty TypeArgument where
  ppr (ReferenceArgument ref_type) = ppr ref_type
  ppr (WildcardArgument w)         = ppr w

instance Pretty Wildcard where
  ppr (Wildcard m_bounds) = "?" ++ pp_bounds
    where
      pp_bounds = case m_bounds of
        Nothing -> ""
        Just b  -> " " ++ ppr b

instance Pretty WildcardBounds where
  ppr (ExtendsWB ref_type) = "extends " ++ ppr ref_type
  ppr (SuperWB ref_type)   = "super " ++ ppr ref_type

instance Pretty ArrayType where
  ppr (PrimArray prim_ty dims) = ppr prim_ty ++ ppr dims
  ppr (COIArray coi dims)      = ppr coi ++ ppr dims

instance Pretty Dims where
  ppr (Dims n_brackets) = concat (replicate n_brackets "[]")
