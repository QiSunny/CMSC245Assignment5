-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Char
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Functions working with Java characters.
--
----------------------------------------------------------------------------

module Language.Java.Char where

import Data.Char
import Data.Word

-- | Look through the string input for Java octal escapes and insert 'o's into
-- them so that they become Haskell octal escapes
insertOctalOEscapes :: String -> String
insertOctalOEscapes ('\\' : '\\' : rest)
  = '\\' : '\\' : insertOctalOEscapes rest -- otherwise, we mistreat "\\123"
insertOctalOEscapes ('\\' : dig : rest)
  | isDigit dig
  = '\\' : 'o' : dig : insertOctalOEscapes rest
insertOctalOEscapes (c:cs) = c : insertOctalOEscapes cs
insertOctalOEscapes []     = []

-- | a Java character is a 16-bit word
type JChar = Word16

-- | Java characters hold only 16 bits. Haskell characters hold more. This
-- converts from a Haskell character to a Java one.
charHtoJ :: Char -> JChar
charHtoJ = fromIntegral . ord

-- | The inverse of 'charHtoJ'
charJtoH :: JChar -> Char
charJtoH = chr . fromIntegral

-- | Useful for manipulating strings (e.g., concatenation)
newtype JString = JString [JChar]

instance Show JString where
  show (JString chars) = show (map charJtoH chars)

-- | Convert a Haskell string to a Java one
stringHtoJ :: String -> JString
stringHtoJ = JString . map charHtoJ

-- | Convenient abbreviation
jshow :: Show a => a -> JString
jshow = stringHtoJ . show

appendStrings :: JString -> JString -> JString
appendStrings (JString s1) (JString s2) = JString (s1 ++ s2)
