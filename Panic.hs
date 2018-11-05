-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Panic
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Exports the panic function to abort the interpreter.
--
----------------------------------------------------------------------------

module Language.Java.Panic ( panic, unimplemented ) where

-- | Aborts the program. Use only when some precondition has not been met.
-- This should *not* be used when the user types in an erroneous program.
panic :: String -> a
panic str = error $ "Panic! I thought this could never happen: " ++ str

-- | Aborts the program. Use when some Java feature has not yet been
-- implemented. Pass a string like "lex hexadecimal literals", beginning
-- with a verb.
unimplemented :: String -> a
unimplemented str = error $ "Oops! I don't yet know how to " ++ str
