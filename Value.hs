{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Value
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Values in Java
--
----------------------------------------------------------------------------

module Language.Java.Value where

import Language.Java.Char
import Language.Java.Type
import Language.Java.Panic

import Data.Int

-- | Java values, as can be, e.g., stored in a variable.
-- See S4.2, S4.3
data Value
  = ByteV Int8
  | ShortV Int16
  | IntV Int32
  | LongV Int64
  | CharV JChar
  | FloatV Float
  | DoubleV Double
  | BooleanV Bool
  | ReferenceV Reference
  deriving (Show)

-- | A Java Reference is either Null or a pointer into the heap; heap
-- locations are denoted by Ints.
data Reference
  = Null
  | Pointer Int       -- The Int is incremented by 1 for every pointer allocated
  deriving (Eq, Show) -- There is no garbage collection
  -- NB: Derive Eq, as equality between references is a good thing to compute

-- | Get a default value of a type. See S4.12.5.
defaultValueOfType :: Type -> Value
defaultValueOfType (PrimType prim) = defaultValueOfPrimType prim
defaultValueOfType (RefType _)     = ReferenceV Null

-- | Get a default value of a primitive type. See S4.12.5.
defaultValueOfPrimType :: PrimitiveType -> Value
defaultValueOfPrimType Byte     = ByteV 0
defaultValueOfPrimType Short    = ShortV 0
defaultValueOfPrimType Int      = IntV 0
defaultValueOfPrimType Long     = LongV 0
defaultValueOfPrimType Char     = CharV 0
defaultValueOfPrimType Float    = FloatV 0
defaultValueOfPrimType Double   = DoubleV 0
defaultValueOfPrimType Boolean  = BooleanV False
defaultValueOfPrimType NullType = panic "defaultValueOfPrimType <nulltype>"

-- | Value types are Haskell types that correspond to Java values
class ValueType a where
  -- | Encode the argument in a Java value
  toValue :: a -> Value

instance ValueType Int8 where
  toValue = ByteV

instance ValueType Int16 where
  toValue = ShortV

instance ValueType Int32 where
  toValue = IntV

instance ValueType Int64 where
  toValue = LongV

instance ValueType JChar where
  toValue = CharV

instance ValueType Float where
  toValue = FloatV

instance ValueType Double where
  toValue = DoubleV

instance ValueType Bool where
  toValue = BooleanV
