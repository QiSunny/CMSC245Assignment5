-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.PrettyM
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines the class PrettyM, used for pretty-printing internal AST.
-- This differs from Show in that Pretty output is meant for users, not
-- implementors. ('Show' shows the internal syntax.)
--
-- PrettyM differs from Pretty in that the former allows computations
-- in the Java monad, with access to IO and the Java heap. This is nice
-- for entities that can contain references.
--
-- Note: This is not a full pretty-printer -- just pretty strings, really.
--
----------------------------------------------------------------------------

module Language.Java.PrettyM where

import Language.Java.Char
import Language.Java.Lang
import Language.Java.Monad
import Language.Java.Mutate
import Language.Java.Pretty
import Language.Java.State
import Language.Java.String
import Language.Java.Value

import qualified Data.Map    as M
import qualified Data.Vector as V
import Data.List

-- | How many references should we look through when pretty-printing?
defaultHeapUnrollCount :: Int
defaultHeapUnrollCount = 2

-- | This class includes all entities which need access to the heap
-- (and/or 'IO') in order to be printed.
class PrettyM a where
  pprM :: Int  -- ^ how many more references to look through
       -> a -> Java String
   -- The Int we pass in starts at some small number (the default is 2)
   -- that says how many references to look through when printing objects.
   -- If we don't have such a number, then pretty-printing a (say) circular
   -- linked list would cause the interpreter to crash.

instance PrettyM HeapElement where
  pprM r (ObjectHE obj) = pprM r obj
  pprM r (ArrayHE arr)  = pprM r arr

instance PrettyM Object where
  pprM r (Object { obj_class  = clas
                 , obj_fields = fields }) = do
    field_strs <- mapM mk_field_str (M.assocs fields)
    pure (ppr clas ++ " { " ++ intercalate ", " field_strs ++ " }")
    where
      mk_field_str (name, value) = do
        val_str <- pprM r value
        pure (ppr name ++ " = " ++ val_str)

instance PrettyM Array where
  pprM r (Array { ar_element_type = elt_ty
                , ar_length       = len
                , ar_data         = vec }) = do
    imm_vec       <- ioAction (V.freeze vec)
    contents_strs <- mapM (pprM r) (V.toList imm_vec)
    let pp_contents
          | len == 0  = "{}"
          | otherwise = "{ " ++ intercalate ", " contents_strs ++ " }"
    pure (ppr elt_ty ++ "[" ++ show len ++ "] " ++ pp_contents)

instance PrettyM Value where
    -- How values are printed is never specified. Let's put on suffixes so
    -- that the reader can know the type
  pprM _ (ByteV n)        = pure $ show n ++ "b"
  pprM _ (ShortV n)       = pure $ show n ++ "s"
  pprM _ (IntV n)         = pure $ show n
  pprM _ (LongV n)        = pure $ show n ++ "l"
  pprM _ (CharV n)        = pure $ show (charJtoH n)
  pprM _ (FloatV n)       = pure $ show n ++ "f"
  pprM _ (DoubleV n)      = pure $ show n
  pprM _ (BooleanV True)  = pure "true"
  pprM _ (BooleanV False) = pure "false"
  pprM r (ReferenceV ref) = pprM r ref

instance PrettyM Reference where
  pprM _ Null        = pure "null"
  pprM r (Pointer n) = do
    contents <- if r == 0
                then pure ""
                else do
                  heap_elt <- dereferenceRaw n
                  contents_str <- case heap_elt of
                          -- this is a special case for strings, because it's
                          -- so convenient to see them rendered nicely
                    ObjectHE obj
                      | isStringObject obj
                      -> do str <- getString obj
                            pure (show str)

                     -- non-String case
                    _ -> pprM (r-1) heap_elt

                   -- Here, both the String case and the general case combine again
                   -- this is where we decrement r
                  pure (" => " ++ contents_str)

    pure ("@" ++ show n ++ contents)
