-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Mutate
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for mutating the global Java state
--
----------------------------------------------------------------------------

module Language.Java.Mutate where

import Language.Java.Monad
import Language.Java.Panic
import Language.Java.State
import Language.Java.Type
import Language.Java.Value

import qualified Data.IntMap         as IM
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import Control.Monad

-- | Put a HeapElement into the heap, returning a (non-null) reference to that
-- object. Never returns Null.
alloc :: HeapElement -> Java Reference
alloc he = do
    -- retrieve the existing state from the monad
  jst@JavaState { jst_heap     = old_heap
                , jst_next_ptr = ptr_val } <- getState
  let new_heap = IM.insert ptr_val he old_heap

        -- the new state is just like the old state, but with an updated heap.
        -- This syntax is called "record update" syntax, and it modifies just
        -- some of the fields of a record.
      new_jst  = jst { jst_heap     = new_heap
                     , jst_next_ptr = ptr_val + 1 }

  setState new_jst

  pure (Pointer ptr_val)

-- | Retrieve a heap element from a raw pointer value. Panics if the pointer
-- is invalid.
dereferenceRaw :: Int -> Java HeapElement
dereferenceRaw ptr = do
  JavaState { jst_heap = heap } <- getState
  case IM.lookup ptr heap of
    Just he -> pure he
    Nothing -> panic $ "Invalid pointer: " ++ show ptr ++ "\n" ++
                       "Heap: " ++ show heap
     -- The Nothing case really is a panic, not a NullPointerException.
     -- We're in the case where we're looking up a proper, non-null pointer.
     -- There should never be a missing heap element there.

-- | Retrieve a heap element from a raw pointer value. Issues an error if the
-- pointer is invalid.
dereferenceRawChecked :: Int -> Java HeapElement
dereferenceRawChecked ptr = do
  JavaState { jst_next_ptr = heap_limit } <- getState
  when (ptr < initialHeapPtr) (issueError $ "Heap pointer too low: " ++ show ptr)
  when (ptr >= heap_limit) (issueError $ "Heap pointer too high: " ++ show ptr)
  dereferenceRaw ptr

-- | Dereference a Java reference. Throws NullPointerException if the reference
-- is null.
dereference :: Reference -> Java HeapElement
dereference Null = throwJavaException "NullPointerException"
  -- maybe someday we'll handle exceptions
dereference (Pointer n) = dereferenceRaw n

-- | Build an array of the desired type and length, returning the Array record
-- describing it. Does *not* allocate memory in the Java heap.
-- As always in Java, the array is initialized to the default value of the type
-- provided. See S4.12.5.
mkArray :: Type -> Int -> Java Array
mkArray ty len = do
  vec <- ioAction $ VM.replicate len (defaultValueOfType ty)
  pure $ Array { ar_element_type = ty
               , ar_length       = len
               , ar_data         = vec }

-- | Build an array from a list of values that it should start with.
-- This does *not* allocate memory in the Java heap.
mkArrayFromList :: Type -> [Value] -> Java Array
mkArrayFromList ty vals = do
  vec <- ioAction $ V.thaw (V.fromList vals)
  pure $ Array { ar_element_type = ty
               , ar_length       = VM.length vec  -- faster than getting (length vals)
               , ar_data         = vec }

-- | Throw a Java exception. Right now, just issues an error to abort the
-- interpreter, but perhaps we will do the right Java thing in the future.
throwJavaException :: String -> Java a
throwJavaException = issueError
