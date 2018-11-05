-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.State
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- A datatype and functions for tracking global state in a Java interpreter
--
----------------------------------------------------------------------------

module Language.Java.State where

import Language.Java.Identifier
import Language.Java.Type
import Language.Java.Value

import qualified Data.IntMap         as IM
import qualified Data.Map            as M
import qualified Data.Vector.Mutable as V

---------------------------------------------
-- The interpreter's internal state

type Heap = IM.IntMap HeapElement

data JavaState
  = JavaState { jst_heap     :: Heap   -- mapping from pointers to heap elements
              , jst_next_ptr :: Int    -- lowest free pointer
              }
  deriving (Show)
{- Why use an IntMap to store the heap? Because it needs to grow. Haskell does not have
a popular implementation of something like Java's ArrayList, a numerical-indexed structure
that can grow and has very quick access times. (A list would be terrible terrible here.)
The closest thing appears to be Data.ArrayList.Generic from the impure-containers package.
That would work, but IntMap is much more standard in Haskell. -}

initialHeapPtr :: Int
initialHeapPtr = 1  -- could start at 0, but that would be confusing for C folks

initialState :: JavaState
initialState = JavaState { jst_heap = IM.empty
                         , jst_next_ptr = 1
                         }

---------------------------------------------
-- Heap objects

-- | An element stored on the heap.
data HeapElement
  = ObjectHE Object
  | ArrayHE Array
  deriving (Show)

-- | An instance of a class, stored on the heap
data Object
  = Object { obj_class  :: ClassType    -- the runtime type of the object
           , obj_fields :: M.Map Identifier Value  -- field values
           }
  deriving (Show)

data Array
  = Array { ar_element_type :: Type       -- runtime type of elements
          , ar_length       :: Int        -- length of array
          , ar_data         :: V.IOVector Value  -- array data, stored as a Haskell array
             -- See Note [Representation of arrays]
          }

-- IOVector has no Show instance, so we have to write this manually in order
-- to suppress showing the vector
instance Show Array where
  show (Array { ar_element_type = ty
              , ar_length       = len })
    = "Array { ar_element_type = " ++ show ty ++ ", " ++
      "ar_length = " ++ show len ++ ", ar_data = { ... } }"

{-
Note [Representation of arrays]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are several independent challenges in dealing with the representation of arrays.
I suppose this is natural, because arrays are such a fundamentally *imperative*
structure, so they are a poor fit for Haskell.

The fundamental issue is whether the array should be stored in a pure structure or
an effectful one. By "pure" here, I mean one that can be updated without the use of
a monad. Of course, in the pure fragment of Haskell (that is, the parts that don't
use IO somehow), there is no way to "update" a structure. All you can do is make
an almost-copy, where the almost-copy has the change you want. For most structures,
such as Map and IntMap, this is fine. The structure is designed in such a way that
you have to touch only a logarithmic fraction of the structure to perform an update.
(In other words, for a structure that has 1,000,000 elements, you would only have to
take roughly 20 steps to update one element.) However, an array doesn't have the right
structure (it's not a tree), and so updating a 1,000,000 element array would take
1,000,000 steps -- utterly unacceptable. Any linear algorithm over arrays in the
user's code would become quadratic or worse. Thus, the easy answer -- just use
a Haskell array (called Vector, from the Data.Vector module in the vector package) --
doesn't work.

We then seem to have two options: use an IntMap or use an IOVector. An IntMap is a
nice pure structure with the right interface (that is, we can look up values given
integer indices). It's performant. But it's not quite as performant as a Java array
should be: lookups and updates still take O(log(n)) time (where n is the length
of the array), not the O(1) time they should. This is probably acceptable in practice
(given that this isn't going to be a real interpreter), but still deeply dissatisfying.
It's also patently absurd to represent a simple structure like an array with a
tree!

IOVector is a variant of Vector that works with the IO monad. Because it works with
IO, operations on an IOVector can be effectful -- that is, they can update values
in-place without copying. Both accessing and updating elements in an IOVector run
in O(1) time. Hooray. And, since the Java monad we're using throughout the interpreter
has access to IO (through the ioAction :: IO a -> Java a conversion function),
we can use IOVector quite successfully.

This does mean that we can access array elements only in a monadic setting, but
that's fine -- we have access to the Java monad everywhere we need to. It's
unfortunate to rely on IO in this way, but it seems unavoidable here.

Separate from all the above, there is one other minor annoyances about the
array representation:

Suppose we have an int[]. Accessing every element requires looking at a Value
and checking it to be IntV. This is silly. Really, we should have 9 variants
of Array, one for each primitive type, and then one for reference types.
But that's a pain, so we're not doing that.
-}
