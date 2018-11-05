-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Lang
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Definitions of entities (like String) that are in-scope and ready-to-go
-- from the start
--
----------------------------------------------------------------------------

module Language.Java.Lang where

import Language.Java.Identifier
import Language.Java.Panic
import Language.Java.State
import Language.Java.Type
import Language.Java.Value

import qualified Data.Map as M

---------------------------------------------------------------------
-- Strings

{-
Someday, Strings will be loaded in via a .java file. Until that day,
they are described here:

class String
{
  char[] value;
}

That is, a String object has a field named "value" that is an array of
the characters in the string. Naming the field "value" comes from
http://hg.openjdk.java.net/jdk8/jdk8/jdk/file/687fd7c7986d/src/share/classes/java/lang/String.java
which I hope to incorporate someday.

If/when this changes, update allocString in Language.Java.String.
-}

stringType :: Type
stringType = RefType (COIRT stringCOIType)

stringCOIType :: COIType
stringCOIType = COIType Nothing (Identifier "String") []

stringValueId :: Identifier
stringValueId = Identifier "value"

isStringType :: Type -> Bool
isStringType (RefType (COIRT coi)) = isStringCOIType coi
isStringType _                     = False

isStringCOIType :: COIType -> Bool
isStringCOIType (COIType Nothing (Identifier "String") []) = True
isStringCOIType _                                          = False

isStringObject :: Object -> Bool
isStringObject (Object { obj_class = string }) = isStringCOIType string

isStringHeapElement :: HeapElement -> Bool
isStringHeapElement (ObjectHE obj) = isStringObject obj
isStringHeapElement _              = False

---------------------------------------------------------------------
-- Boxed values

boxedValueField :: Identifier
boxedValueField = Identifier "value"

-- | Gets the primitive type associated with the boxed type provided. If the
-- type provided is not a boxed type, returns Nothing.
unboxType :: ReferenceType -> Maybe PrimitiveType
unboxType (COIRT (COIType Nothing (Identifier name) []))
  | name == "Byte"       = Just Byte
  | name == "Short"      = Just Short
  | name == "Integer"    = Just Int
  | name == "Long"       = Just Long
  | name == "Character"  = Just Char
  | name == "Float"      = Just Float
  | name == "Double"     = Just Double
  | name == "Boolean"    = Just Boolean
unboxType _ = Nothing

-- | Extracts the value from one of the "box" objects (Byte, Short, Character,
-- Integer, Long, Float, Double, Boolean). Panics or otherwise fails miserably
-- if the Object is not one of these types.
-- Post-condition: the value is a primitive value.
unboxObject :: Object -> Value
unboxObject obj@(Object { obj_fields = fields })
  = case M.lookup boxedValueField fields of
      Just val -> val
      Nothing  -> panic $ "unboxObject can't find a 'value' field: " ++ show obj
