-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.String
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions to work with Java Strings and chars
--
----------------------------------------------------------------------------

module Language.Java.String where

import Language.Java.Char
import Language.Java.Lang
import Language.Java.Monad
import Language.Java.Mutate
import Language.Java.Panic
import Language.Java.State
import Language.Java.Type
import Language.Java.Value

import qualified Data.Map    as M
import qualified Data.Vector as V

-- | Extracts a Haskell string from a Java String object.
-- Precondition: the Object really is a String object.
getString :: Object -> Java JString
getString (Object { obj_fields = string_fields })
  = case M.lookup stringValueId string_fields of
      Just (ReferenceV (Pointer chars_ptr)) -> do
        chars <- dereferenceRaw chars_ptr
        case chars of
          ArrayHE (Array { ar_data = vec }) -> do
            -- this "freeze" step is part of how we
            -- convert an IOVector to a list
            imm_vec <- ioAction $ V.freeze vec
            pure (JString $ map (\(CharV jch) -> jch) (V.toList imm_vec))

          -- this happens if the string doesn't actually
          -- contain an array
          _ -> panic $ "Unexpected String contents: " ++ show chars

      -- this case is when a string doesn't contain a value field
      _ -> panic $ "Expected a non-null value field here: " ++ show string_fields

-- | Build a reference to a String
allocString :: JString -> Java Reference
allocString (JString str) = do
  -- See Language.Java.Lang for information about the internal organization of
  -- strings.
  value_arr <- mkArrayFromList (PrimType Char) (map CharV str)
  array_ref <- alloc (ArrayHE value_arr)
  let obj = Object { obj_class = stringCOIType
                   , obj_fields = M.singleton stringValueId (ReferenceV array_ref) }
  alloc (ObjectHE obj)
