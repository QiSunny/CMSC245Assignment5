{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Parser
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- A Java parser
--
----------------------------------------------------------------------------

{- There are many stones left unturned here. There are principally two reasons
for the TODOs in this file.

1. Java is a big language, and it's simply not all here yet.

2. The grammar as presented in the JLS is ambiguous in an LALR parser. LALR means
that the parser reads tokens from left to right (that's the LR), with one token
of lookahead (that's the LA). It thus has to commit to a particular parsing choice
by looking only at the next token. The conflicts below describe problems this
poses for parsing Java.

conflict1: The problem is that all the following are valid:

  obj.field           // normal field access
  obj.field1.field2   // nested field access
  Type.this           // access of outer 'this' from inner class
  Outer.Inner.this    // access of outer 'this' when Inner is an inner class within Outer
  Type.class          // a "class" constant for a type
  Outer.Inner.class   // a "class" constant for Inner, an inner class of Outer
  ...                 // and a few more

When we get to the last dot, we need to decide if we're parsing something like the first
two examples, or something like the last 4. The ".this" and ".class" syntax are both
special syntax, treated differently from normal identifiers. So we have to make the
decision before the dot, and we can't.

The solution to this is to parse a list of dot-separated words into a list, then check
whether the last word is special. Not so hard, but I'd like to keep the parser here
closer to the JLS syntax, and so I'm not doing that now, as the forms I've eliminated
are somewhat exotic.

conflict2: Lambda expressions are hard to parse. Specifically, if we look
at "(x)", is that the beginning of a lambda? Or is it an expression in parentheses?
Impossible to know without more lookahead. Not sure offhand what the best solution
is here. Could ban this syntax (if there were a type there, it wouldn't be
ambiguous) or so something else creative. Deferring until later.

conflict3: This one is about telling the difference between casts and expressions.
If we have "(blah)", is that a parenthesized expression? Or is it the start of a cast?
Impossible to know. I think the solution here is to combine the ClassOrInterfaceType
parser with the QualifiedName parser. That's not too bad, but -- again -- I don't
want to stray from the JLS if possible. Right now, we don't support sub-typing yet,
so casting by a reference type isn't necessary.
-}

{-
Separate from the *removed* conflicts above, there remain some existing, more-or-less
intentional conflicts. These are all shift/reduce conflicts. These come up when
the parser is conflicted between ending a rule (reducing) or continuing a rule
(shifting). Shift/reduce conflicts are always resolved in favor of shifting: that is,
the rules consume as much of the input as possible. (Some call this "greedy"
parsing.) Because the resolution of the conflict is predictable, it's acceptable
to leave the conflict in.

Conflicts: None at this time.

-}

module Language.Java.Parser ( parseStatement, parseExpression ) where

import Language.Java.Identifier
import Language.Java.Literal
import Language.Java.Type
import Language.Java.Token
import Language.Java.Syntax
import Language.Java.Monad

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty((:|)), cons )
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn5 :: (Identifier) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Identifier)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Literal) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Literal)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (PrimitiveType) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (PrimitiveType)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (PrimitiveType) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (PrimitiveType)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (PrimitiveType) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (PrimitiveType)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (PrimitiveType) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (PrimitiveType)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (ReferenceType) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (ReferenceType)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (COIType) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (COIType)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (ArrayType) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (ArrayType)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Dims) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Dims)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([TypeArgument]) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([TypeArgument])
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([TypeArgument]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([TypeArgument])
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (TypeArgument) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (TypeArgument)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Wildcard) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Wildcard)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (WildcardBounds) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (WildcardBounds)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (QualifiedName) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (QualifiedName)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (NonEmpty Identifier) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (NonEmpty Identifier)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Statement) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Statement)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Expression) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Expression)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Expression) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Expression)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Expression) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Expression)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (ArrayAccess) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (ArrayAccess)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (FieldAccess) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (FieldAccess)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Expression) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Expression)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Expression) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Expression)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Expression) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Expression)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Expression) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Expression)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Expression) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Expression)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Expression) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Expression)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Expression) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Expression)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Expression) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Expression)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Expression) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Expression)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Expression) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Expression)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Expression) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Expression)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Expression) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Expression)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Expression) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Expression)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (Expression) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (Expression)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Expression) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Expression)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Expression) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Expression)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Expression) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Expression)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (Expression) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (Expression)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (Expression) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (Expression)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Expression) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Expression)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (Expression) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (Expression)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (LeftHandSide) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (LeftHandSide)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (AssignmentOperator) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (AssignmentOperator)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x70\x80\x08\x22\x40\x02\x0c\x00\x00\x00\x00\x00\x00\x00\x0e\x10\x41\x04\x48\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x0a\x00\x00\x00\x00\x00\x00\x00\x01\x00\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x44\x44\x44\xa4\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\xfd\x00\x11\x44\x80\x04\x18\x00\x00\x00\x00\x00\x00\x00\x1c\x20\x82\x08\x90\x00\x03\x00\x00\x00\x00\x00\x00\x80\x03\x44\x10\x01\x12\x60\x00\x00\x00\x00\x00\x00\x00\x70\x80\x08\x22\x40\x02\x0c\x00\x00\x00\x00\x00\x00\x00\x0e\x10\x41\x04\x48\x80\x01\x00\x00\x00\x00\x00\x00\xc0\x01\x22\x88\x00\x09\x30\x00\x00\x00\x00\x00\x00\x00\x38\x40\x04\x11\x20\x01\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x1c\x20\x82\x08\x90\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x40\x04\x11\x20\x01\x06\x00\x00\x00\x00\x00\x00\x00\x07\x88\x20\x02\x24\xc0\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x11\x44\x80\x04\x18\x00\x00\x00\x00\x00\x00\x00\x1c\x20\x82\x08\x90\x00\x03\x00\x00\x00\x00\x00\x00\x80\x03\x44\x10\x01\x12\x60\x00\x00\x00\x00\x00\x00\x00\x70\x80\x08\x22\x40\x02\x0c\x00\x00\x00\x00\x00\x00\x00\x0e\x10\x41\x04\x48\x80\x01\x00\x00\x00\x00\x00\x00\xc0\x01\x22\x88\x00\x09\x30\x00\x00\x00\x00\x00\x00\x5e\x07\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x07\x88\x20\x02\x24\xc0\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x11\x44\x80\x04\x18\x00\x00\x00\x00\x00\x00\x00\x1c\x20\x82\x08\x90\x00\x03\x00\x00\x00\x00\x00\x00\x80\x03\x44\x10\x01\x12\x60\x00\x00\x00\x00\x00\x00\x00\x70\x80\x08\x22\x40\x02\x0c\x00\x00\x00\x00\x00\x00\x00\x0e\x10\x41\x04\x48\x80\x01\x00\x00\x00\x00\x00\x00\xc0\x01\x22\x88\x00\x09\x30\x00\x00\x00\x00\x00\x00\x00\x38\x40\x04\x11\x20\x01\x06\x00\x00\x00\x00\x00\x00\x00\x07\x88\x20\x02\x24\xc0\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x11\x44\x80\x04\x18\x00\x00\x00\x00\x00\x00\x00\x1c\x20\x82\x08\x90\x00\x03\x00\x00\x00\x00\x00\x00\x80\x03\x44\x10\x01\x12\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x01\x22\x88\x00\x09\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x11\x44\x80\x04\x18\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x02\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x60\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x20\x82\x08\x90\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x80\x08\x22\x40\x02\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x03\x00\x00\x80\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xeb\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x78\x1d\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\xaf\x03\x00\x00\x80\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseStatement","%start_parseExpression","Identifier","Literal","PrimitiveType","NumericType","IntegralType","FloatingPointType","ReferenceType","ClassOrInterfaceType","ArrayType","Dims","TypeArguments","TypeArgumentList","TypeArgument","Wildcard","WildcardBounds","QualifiedName","ReversedDottedIdentifiersNE","Statement","Expression","Primary","PrimaryNoNewArray","ArrayAccess","FieldAccess","PostfixExpression","PostIncrementExpression","PostDecrementExpression","UnaryExpression","PreIncrementExpression","PreDecrementExpression","UnaryExpressionNotPlusMinus","CastExpression","MultiplicativeExpression","AdditiveExpression","ShiftExpression","RelationalExpression","EqualityExpression","AndExpression","ExclusiveOrExpression","InclusiveOrExpression","ConditionalAndExpression","ConditionalOrExpression","ConditionalExpression","AssignmentExpression","Assignment","LeftHandSide","AssignmentOperator","'boolean'","'byte'","'char'","'double'","'extends'","'float'","'instanceof'","'int'","'long'","'short'","'super'","'this'","'('","')'","'['","']'","','","'.'","'<'","'='","'=='","'+'","'+='","'>'","'>='","'-'","'-='","' <'","'<='","'*'","'*='","'!'","'!='","'/'","'/='","'~'","'&&'","'&'","'&='","'?'","'||'","'|'","'|='","':'","'++'","'^'","'^='","'--'","'%'","'m='","'<<'","'<<='","'>>'","'>>='","'>>>'","'>>>='","IDENTIFIER","LITERAL","%eof"]
        bit_start = st * 109
        bit_end = (st + 1) * 109
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..108]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x3f\x00\x3f\x00\xc8\xff\x00\x00\x00\x00\x00\x00\xd0\x01\xf4\xff\xcd\xff\x16\x00\x13\x00\xcf\x01\x82\x02\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\x09\x00\xfd\xff\x0e\x00\x08\x00\x07\x00\x0b\x00\x02\x00\x12\x00\xe7\xff\x00\x00\x00\x00\x00\x00\x83\x02\x23\x00\x00\x00\x01\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x00\x00\x05\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3b\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x3f\x00\x00\x00\x00\x00\x3f\x00\x0f\x00\x0f\x00\x3f\x00\x41\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xfe\xff\x09\x00\x09\x00\x09\x00\xfd\xff\xfd\xff\xfd\xff\xfd\xff\x3c\x00\x44\x00\x00\x00\x3e\x00\x00\x00\x0e\x00\x0e\x00\x08\x00\x28\x00\x26\x00\x2c\x00\x32\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x3f\x00\x00\x00\x48\x00\x22\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x45\x00\x4b\x00\x00\x00\x19\x00\x4d\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x3b\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xa7\x00\xd4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x00\x00\x7a\x00\x26\x04\x37\x04\x48\x04\x59\x04\x6a\x04\x7b\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x01\xfc\x01\x24\x02\x4b\x02\x71\x02\xaf\x02\xd3\x02\xf6\x02\x62\x00\x19\x03\x3b\x03\x5d\x03\x7f\x03\xa1\x03\xc2\x03\xe3\x03\x04\x04\x15\x04\x8c\x04\x9d\x04\xae\x04\x00\x00\x00\x00\x5b\x01\x6d\x00\x6e\x00\x88\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\x68\x00\x00\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x04\x00\x00\xb5\x01\x00\x00\x00\x00\x73\x00\x00\x00\x00\x00\xde\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x67\x00\x6c\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\xa9\x00\xec\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\xfd\xff\xdc\xff\xd7\xff\xcd\xff\xdd\xff\x00\x00\xce\xff\xd8\xff\xd3\xff\xd4\xff\xc1\xff\xcc\xff\xcb\xff\xbc\xff\xc8\xff\xc7\xff\xc4\xff\xbe\xff\xb8\xff\xb5\xff\xb1\xff\xab\xff\xa8\xff\xa6\xff\xa4\xff\xa2\xff\xa0\xff\x9e\xff\x9c\xff\xd9\xff\x9b\xff\x00\x00\x00\x00\xd6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\xda\xff\xcd\xff\xd3\xff\xd4\xff\xc2\xff\xc3\xff\xc0\xff\xbf\xff\xc5\xff\xc6\xff\x00\x00\xfb\xff\xf9\xff\xf8\xff\x00\x00\xfa\xff\xf7\xff\xf3\xff\xf1\xff\xf2\xff\xf5\xff\xf4\xff\xf6\xff\x00\x00\x00\x00\x96\xff\x92\xff\x91\xff\x95\xff\x94\xff\x8d\xff\x8b\xff\x8c\xff\x93\xff\x90\xff\x8f\xff\x8e\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\xff\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xd0\xff\x00\x00\xb9\xff\xba\xff\xbb\xff\xb6\xff\xb7\xff\xb2\xff\xb3\xff\xb4\xff\xae\xff\xb0\xff\xad\xff\xaf\xff\xee\xff\x00\x00\xac\xff\xf0\xff\xef\xff\xa9\xff\xaa\xff\xa7\xff\xa5\xff\xa3\xff\xa1\xff\x9f\xff\x00\x00\x9a\xff\xcf\xff\xd5\xff\x00\x00\xbd\xff\x00\x00\xe9\xff\x00\x00\x00\x00\xea\xff\xed\xff\x00\x00\xd1\xff\xd2\xff\xe3\xff\x00\x00\xe5\xff\xe2\xff\xe1\xff\xec\xff\xe8\xff\x9d\xff\xe7\xff\xeb\xff\xe0\xff\x00\x00\x00\x00\x00\x00\xe6\xff\xe4\xff\xde\xff\xdf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x39\x00\x01\x00\x02\x00\x03\x00\x04\x00\x12\x00\x06\x00\x3b\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x28\x00\x29\x00\x01\x00\x02\x00\x03\x00\x04\x00\x07\x00\x06\x00\x16\x00\x08\x00\x09\x00\x0a\x00\x1a\x00\x1e\x00\x15\x00\x05\x00\x16\x00\x22\x00\x20\x00\x0f\x00\x1a\x00\x0b\x00\x24\x00\x18\x00\x19\x00\x12\x00\x21\x00\x1c\x00\x1d\x00\x2a\x00\x26\x00\x2d\x00\x31\x00\x33\x00\x30\x00\x35\x00\x2d\x00\x37\x00\x12\x00\x30\x00\x25\x00\x28\x00\x2e\x00\x39\x00\x3a\x00\x01\x00\x02\x00\x03\x00\x04\x00\x3b\x00\x06\x00\x0f\x00\x08\x00\x09\x00\x0a\x00\x0e\x00\x0e\x00\x39\x00\x39\x00\x0b\x00\x0c\x00\x0d\x00\x0f\x00\x26\x00\x13\x00\x12\x00\x10\x00\x10\x00\x0f\x00\x2e\x00\x16\x00\x2a\x00\x25\x00\x10\x00\x1a\x00\x2c\x00\x39\x00\x11\x00\x18\x00\x0f\x00\x20\x00\x13\x00\x2d\x00\x00\x00\x24\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x30\x00\x0a\x00\x09\x00\x09\x00\x00\x00\x39\x00\x0e\x00\x0a\x00\xff\xff\x39\x00\x3a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x00\x00\x09\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x0f\x00\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\x00\x00\xff\xff\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x0f\x00\xff\xff\xff\xff\xff\xff\x14\x00\x14\x00\xff\xff\x17\x00\x17\x00\xff\xff\xff\xff\x1b\x00\x1b\x00\xff\xff\xff\xff\x1f\x00\x1f\x00\xff\xff\xff\xff\x23\x00\x23\x00\xff\xff\xff\xff\x27\x00\x27\x00\xff\xff\xff\xff\x2b\x00\x2b\x00\x00\x00\x01\x00\x2f\x00\x2f\x00\xff\xff\x32\x00\x32\x00\x34\x00\x34\x00\x36\x00\x36\x00\x38\x00\x38\x00\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x14\x00\x14\x00\xff\xff\x17\x00\x17\x00\xff\xff\xff\xff\x1b\x00\x1b\x00\xff\xff\xff\xff\x1f\x00\x1f\x00\xff\xff\xff\xff\x23\x00\x23\x00\xff\xff\xff\xff\x27\x00\x27\x00\xff\xff\xff\xff\x2b\x00\x2b\x00\x00\x00\x01\x00\x2f\x00\x2f\x00\xff\xff\x32\x00\x32\x00\x34\x00\x34\x00\x36\x00\x36\x00\x38\x00\x38\x00\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\xff\xff\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x00\x00\xff\xff\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x04\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x6d\x00\x42\x00\xff\xff\x43\x00\x44\x00\x45\x00\x24\x00\x25\x00\x26\x00\x54\x00\x55\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x5c\x00\x42\x00\x27\x00\x43\x00\x44\x00\x45\x00\x28\x00\x66\x00\x5a\x00\xa5\x00\x64\x00\x67\x00\x29\x00\x6b\x00\x65\x00\xa6\x00\x2a\x00\x5d\x00\x5e\x00\x6c\x00\x5b\x00\x5f\x00\x60\x00\x57\x00\x59\x00\x2b\x00\x68\x00\x61\x00\x2c\x00\x62\x00\x69\x00\x63\x00\x46\x00\x6a\x00\x56\x00\x9e\x00\x58\x00\x04\x00\x2d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\xff\xff\x42\x00\x6e\x00\x43\x00\x44\x00\x45\x00\x8f\x00\x8e\x00\x04\x00\x04\x00\x24\x00\x25\x00\x26\x00\x93\x00\x59\x00\x97\x00\x94\x00\x99\x00\x98\x00\x93\x00\x58\x00\x27\x00\x57\x00\x56\x00\xa0\x00\x28\x00\x91\x00\x04\x00\xa7\x00\xa8\x00\x93\x00\x29\x00\x97\x00\x46\x00\x7e\x00\x2a\x00\x7f\x00\x39\x00\x3a\x00\x3b\x00\x80\x00\x81\x00\x82\x00\x8c\x00\x2b\x00\x70\x00\x6f\x00\x2c\x00\x95\x00\x94\x00\x91\x00\x9e\x00\x04\x00\xa3\x00\xa2\x00\x00\x00\x04\x00\x2d\x00\x04\x00\x05\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x7e\x00\xa1\x00\x7f\x00\x39\x00\x3a\x00\x3b\x00\xaa\x00\x81\x00\x82\x00\x06\x00\x07\x00\x00\x00\x3c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x04\x00\x05\x00\x7e\x00\x00\x00\x7f\x00\x39\x00\x3a\x00\x3b\x00\xa9\x00\x81\x00\x82\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x07\x00\x2d\x00\x2e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x07\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x07\x00\x00\x00\x8b\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x07\x00\x00\x00\x8a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x07\x00\x00\x00\x71\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x07\x00\x00\x00\x6e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xa0\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x97\xff\x99\xff\x00\x00\x97\xff\x99\xff\x00\x00\x00\x00\x97\xff\x99\xff\x00\x00\x00\x00\x97\xff\x99\xff\x00\x00\x00\x00\x97\xff\x99\xff\x00\x00\x00\x00\x97\xff\x99\xff\x00\x00\x00\x00\x97\xff\x99\xff\x04\x00\x05\x00\x97\xff\x99\xff\x00\x00\x97\xff\x99\xff\x97\xff\x99\xff\x97\xff\x99\xff\x97\xff\x99\xff\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x89\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x88\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x87\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x86\x00\x98\xff\x48\x00\x00\x00\x98\xff\x49\x00\x00\x00\x00\x00\x98\xff\x4a\x00\x00\x00\x00\x00\x98\xff\x4b\x00\x00\x00\x00\x00\x98\xff\x4c\x00\x00\x00\x00\x00\x98\xff\x4d\x00\x00\x00\x00\x00\x98\xff\x4e\x00\x04\x00\x05\x00\x98\xff\x4f\x00\x00\x00\x98\xff\x50\x00\x98\xff\x51\x00\x98\xff\x52\x00\x98\xff\x53\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x85\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x84\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x83\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x7d\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x7c\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x7b\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x7a\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x79\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x78\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x77\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x76\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x75\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x37\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x36\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x35\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x34\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x33\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x32\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x74\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x73\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x72\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x2f\x00\x07\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x30\x00\x31\x00\x0d\x00\x0e\x00\x0f\x00\x8f\x00\x11\x00\x12\x00\x13\x00\x14\x00\x7e\x00\x00\x00\x7f\x00\x39\x00\x3a\x00\x3b\x00\x99\x00\x81\x00\x82\x00\x00\x00\x00\x00\x9a\x00\x9b\x00\x9c\x00\x7e\x00\x00\x00\x7f\x00\x39\x00\x3a\x00\x3b\x00\x99\x00\x81\x00\x82\x00\x00\x00\x00\x00\xa8\x00\x9b\x00\x9c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (2, 116) [
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116)
	]

happy_n_terms = 60 :: Int
happy_n_nonterms = 46 :: Int

happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (IdentifierT happy_var_1) -> 
	happyIn5
		 (Identifier happy_var_1
	)}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (LiteralT happy_var_1) -> 
	happyIn6
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  2# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (happy_var_1
	)}

happyReduce_5 = happySpecReduce_1  2# happyReduction_5
happyReduction_5 happy_x_1
	 =  happyIn7
		 (Boolean
	)

happyReduce_6 = happySpecReduce_1  3# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_7 = happySpecReduce_1  3# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_8 = happySpecReduce_1  4# happyReduction_8
happyReduction_8 happy_x_1
	 =  happyIn9
		 (Byte
	)

happyReduce_9 = happySpecReduce_1  4# happyReduction_9
happyReduction_9 happy_x_1
	 =  happyIn9
		 (Short
	)

happyReduce_10 = happySpecReduce_1  4# happyReduction_10
happyReduction_10 happy_x_1
	 =  happyIn9
		 (Int
	)

happyReduce_11 = happySpecReduce_1  4# happyReduction_11
happyReduction_11 happy_x_1
	 =  happyIn9
		 (Long
	)

happyReduce_12 = happySpecReduce_1  4# happyReduction_12
happyReduction_12 happy_x_1
	 =  happyIn9
		 (Char
	)

happyReduce_13 = happySpecReduce_1  5# happyReduction_13
happyReduction_13 happy_x_1
	 =  happyIn10
		 (Float
	)

happyReduce_14 = happySpecReduce_1  5# happyReduction_14
happyReduction_14 happy_x_1
	 =  happyIn10
		 (Double
	)

happyReduce_15 = happySpecReduce_1  6# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (COIRT happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  6# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (ArrayRT happy_var_1
	)}

happyReduce_17 = happySpecReduce_1  7# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (COIType Nothing happy_var_1 []
	)}

happyReduce_18 = happySpecReduce_2  7# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (COIType Nothing happy_var_1 happy_var_2
	)}}

happyReduce_19 = happySpecReduce_3  7# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (COIType (Just happy_var_1) happy_var_3 []
	)}}

happyReduce_20 = happyReduce 4# 7# happyReduction_20
happyReduction_20 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	case happyOut15 happy_x_4 of { happy_var_4 -> 
	happyIn12
		 (COIType (Just happy_var_1) happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_21 = happySpecReduce_2  8# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (PrimArray happy_var_1 happy_var_2
	)}}

happyReduce_22 = happySpecReduce_2  8# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (COIArray happy_var_1 happy_var_2
	)}}

happyReduce_23 = happySpecReduce_2  9# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  happyIn14
		 (Dims 1
	)

happyReduce_24 = happySpecReduce_3  9# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (case happy_var_3 of Dims n -> Dims (n+1)
	)}

happyReduce_25 = happySpecReduce_3  10# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_2
	)}

happyReduce_26 = happySpecReduce_1  11# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ([happy_var_1]
	)}

happyReduce_27 = happySpecReduce_3  11# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_28 = happySpecReduce_1  12# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (ReferenceArgument happy_var_1
	)}

happyReduce_29 = happySpecReduce_1  12# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (WildcardArgument happy_var_1
	)}

happyReduce_30 = happySpecReduce_1  13# happyReduction_30
happyReduction_30 happy_x_1
	 =  happyIn18
		 (Wildcard Nothing
	)

happyReduce_31 = happySpecReduce_2  13# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (Wildcard (Just happy_var_2)
	)}

happyReduce_32 = happySpecReduce_2  14# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (ExtendsWB happy_var_2
	)}

happyReduce_33 = happySpecReduce_2  14# happyReduction_33
happyReduction_33 happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (SuperWB happy_var_2
	)}

happyReduce_34 = happySpecReduce_1  15# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (case happy_var_1 of id :| quals -> QualifiedName (reverse quals) id
	)}

happyReduce_35 = happySpecReduce_1  16# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1 :| []
	)}

happyReduce_36 = happySpecReduce_3  16# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (happy_var_3 `cons` happy_var_1
	)}}

happyReduce_37 = happySpecReduce_1  17# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (ExpressionStatement happy_var_1
	)}

happyReduce_38 = happySpecReduce_1  18# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_39 = happySpecReduce_1  19# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_40 = happySpecReduce_1  20# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (LiteralE happy_var_1
	)}

happyReduce_41 = happySpecReduce_1  20# happyReduction_41
happyReduction_41 happy_x_1
	 =  happyIn25
		 (ThisE NoTQ
	)

happyReduce_42 = happySpecReduce_3  20# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (happy_var_2
	)}

happyReduce_43 = happySpecReduce_1  20# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (FieldAccessE happy_var_1
	)}

happyReduce_44 = happySpecReduce_1  20# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (ArrayAccessE happy_var_1
	)}

happyReduce_45 = happyReduce 4# 21# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (ArrayAccess (NameE happy_var_1) happy_var_3
	) `HappyStk` happyRest}}

happyReduce_46 = happyReduce 4# 21# happyReduction_46
happyReduction_46 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (ArrayAccess happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_47 = happySpecReduce_3  22# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (FieldAccess (ExpressionFAQ happy_var_1) happy_var_3
	)}}

happyReduce_48 = happySpecReduce_3  22# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (FieldAccess (SuperFAQ NoTQ) happy_var_3
	)}

happyReduce_49 = happySpecReduce_1  23# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  23# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (NameE happy_var_1
	)}

happyReduce_51 = happySpecReduce_1  23# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_52 = happySpecReduce_1  23# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_53 = happySpecReduce_2  24# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (UnaryE PostIncrement happy_var_1
	)}

happyReduce_54 = happySpecReduce_2  25# happyReduction_54
happyReduction_54 happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (UnaryE PostDecrement happy_var_1
	)}

happyReduce_55 = happySpecReduce_1  26# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_56 = happySpecReduce_1  26# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_57 = happySpecReduce_2  26# happyReduction_57
happyReduction_57 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (UnaryE UnaryPlus happy_var_2
	)}

happyReduce_58 = happySpecReduce_2  26# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (UnaryE Negation happy_var_2
	)}

happyReduce_59 = happySpecReduce_1  26# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_60 = happySpecReduce_2  27# happyReduction_60
happyReduction_60 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (UnaryE PreIncrement happy_var_2
	)}

happyReduce_61 = happySpecReduce_2  28# happyReduction_61
happyReduction_61 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (UnaryE PreDecrement happy_var_2
	)}

happyReduce_62 = happySpecReduce_1  29# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (happy_var_1
	)}

happyReduce_63 = happySpecReduce_2  29# happyReduction_63
happyReduction_63 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (UnaryE BitwiseNot happy_var_2
	)}

happyReduce_64 = happySpecReduce_2  29# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (UnaryE LogicalNot happy_var_2
	)}

happyReduce_65 = happySpecReduce_1  29# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (happy_var_1
	)}

happyReduce_66 = happyReduce 4# 30# happyReduction_66
happyReduction_66 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn35
		 (CastE (PrimCastType happy_var_2) happy_var_4
	) `HappyStk` happyRest}}

happyReduce_67 = happySpecReduce_1  31# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (happy_var_1
	)}

happyReduce_68 = happySpecReduce_3  31# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (BinaryE happy_var_1 Times happy_var_3
	)}}

happyReduce_69 = happySpecReduce_3  31# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (BinaryE happy_var_1 DividedBy happy_var_3
	)}}

happyReduce_70 = happySpecReduce_3  31# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (BinaryE happy_var_1 Modulus happy_var_3
	)}}

happyReduce_71 = happySpecReduce_1  32# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (happy_var_1
	)}

happyReduce_72 = happySpecReduce_3  32# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (BinaryE happy_var_1 Plus happy_var_3
	)}}

happyReduce_73 = happySpecReduce_3  32# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (BinaryE happy_var_1 Minus happy_var_3
	)}}

happyReduce_74 = happySpecReduce_1  33# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_75 = happySpecReduce_3  33# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (BinaryE happy_var_1 LeftShift happy_var_3
	)}}

happyReduce_76 = happySpecReduce_3  33# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (BinaryE happy_var_1 RightShift happy_var_3
	)}}

happyReduce_77 = happySpecReduce_3  33# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (BinaryE happy_var_1 RightShiftZeroExtension happy_var_3
	)}}

happyReduce_78 = happySpecReduce_1  34# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

happyReduce_79 = happySpecReduce_3  34# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (BinaryE happy_var_1 LessThan happy_var_3
	)}}

happyReduce_80 = happySpecReduce_3  34# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (BinaryE happy_var_1 GreaterThan happy_var_3
	)}}

happyReduce_81 = happySpecReduce_3  34# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (BinaryE happy_var_1 LessThanEquals happy_var_3
	)}}

happyReduce_82 = happySpecReduce_3  34# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (BinaryE happy_var_1 GreaterThanEquals happy_var_3
	)}}

happyReduce_83 = happySpecReduce_3  34# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (InstanceofE happy_var_1 happy_var_3
	)}}

happyReduce_84 = happySpecReduce_1  35# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_85 = happySpecReduce_3  35# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (BinaryE happy_var_1 Equality happy_var_3
	)}}

happyReduce_86 = happySpecReduce_3  35# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (BinaryE happy_var_1 Disequality happy_var_3
	)}}

happyReduce_87 = happySpecReduce_1  36# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_88 = happySpecReduce_3  36# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (BinaryE happy_var_1 BitwiseAnd happy_var_3
	)}}

happyReduce_89 = happySpecReduce_1  37# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_90 = happySpecReduce_3  37# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (BinaryE happy_var_1 BitwiseExclusiveOr happy_var_3
	)}}

happyReduce_91 = happySpecReduce_1  38# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_92 = happySpecReduce_3  38# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (BinaryE happy_var_1 BitwiseOr happy_var_3
	)}}

happyReduce_93 = happySpecReduce_1  39# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_94 = happySpecReduce_3  39# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (BinaryE happy_var_1 LogicalAnd happy_var_3
	)}}

happyReduce_95 = happySpecReduce_1  40# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_96 = happySpecReduce_3  40# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (BinaryE happy_var_1 LogicalOr happy_var_3
	)}}

happyReduce_97 = happySpecReduce_1  41# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_98 = happyReduce 5# 41# happyReduction_98
happyReduction_98 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	case happyOut46 happy_x_5 of { happy_var_5 -> 
	happyIn46
		 (ConditionalE happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_99 = happySpecReduce_1  42# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_100 = happySpecReduce_1  42# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_101 = happySpecReduce_3  43# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_2 of { happy_var_2 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (AssignmentE happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_102 = happySpecReduce_1  44# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (ExpressionNameLHS happy_var_1
	)}

happyReduce_103 = happySpecReduce_1  44# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (FieldAccessLHS happy_var_1
	)}

happyReduce_104 = happySpecReduce_1  44# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (ArrayAccessLHS happy_var_1
	)}

happyReduce_105 = happySpecReduce_1  45# happyReduction_105
happyReduction_105 happy_x_1
	 =  happyIn50
		 (Assigns
	)

happyReduce_106 = happySpecReduce_1  45# happyReduction_106
happyReduction_106 happy_x_1
	 =  happyIn50
		 (TimesAssigns
	)

happyReduce_107 = happySpecReduce_1  45# happyReduction_107
happyReduction_107 happy_x_1
	 =  happyIn50
		 (DividedByAssigns
	)

happyReduce_108 = happySpecReduce_1  45# happyReduction_108
happyReduction_108 happy_x_1
	 =  happyIn50
		 (ModulusAssigns
	)

happyReduce_109 = happySpecReduce_1  45# happyReduction_109
happyReduction_109 happy_x_1
	 =  happyIn50
		 (PlusAssigns
	)

happyReduce_110 = happySpecReduce_1  45# happyReduction_110
happyReduction_110 happy_x_1
	 =  happyIn50
		 (MinusAssigns
	)

happyReduce_111 = happySpecReduce_1  45# happyReduction_111
happyReduction_111 happy_x_1
	 =  happyIn50
		 (ShiftLeftAssigns
	)

happyReduce_112 = happySpecReduce_1  45# happyReduction_112
happyReduction_112 happy_x_1
	 =  happyIn50
		 (ShiftRightAssigns
	)

happyReduce_113 = happySpecReduce_1  45# happyReduction_113
happyReduction_113 happy_x_1
	 =  happyIn50
		 (ShiftRightZeroExtensionAssigns
	)

happyReduce_114 = happySpecReduce_1  45# happyReduction_114
happyReduction_114 happy_x_1
	 =  happyIn50
		 (BitwiseAndAssigns
	)

happyReduce_115 = happySpecReduce_1  45# happyReduction_115
happyReduction_115 happy_x_1
	 =  happyIn50
		 (BitwiseExclusiveOrAssigns
	)

happyReduce_116 = happySpecReduce_1  45# happyReduction_116
happyReduction_116 happy_x_1
	 =  happyIn50
		 (BitwiseOrAssigns
	)

happyNewToken action sts stk [] =
	happyDoAction 59# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	KeywordT BooleanK -> cont 1#;
	KeywordT ByteK -> cont 2#;
	KeywordT CharK -> cont 3#;
	KeywordT DoubleK -> cont 4#;
	KeywordT ExtendsK -> cont 5#;
	KeywordT FloatK -> cont 6#;
	KeywordT InstanceofK -> cont 7#;
	KeywordT IntK -> cont 8#;
	KeywordT LongK -> cont 9#;
	KeywordT ShortK -> cont 10#;
	KeywordT SuperK -> cont 11#;
	KeywordT ThisK -> cont 12#;
	SeparatorT LParenS -> cont 13#;
	SeparatorT RParenS -> cont 14#;
	SeparatorT LBracketS -> cont 15#;
	SeparatorT RBracketS -> cont 16#;
	SeparatorT CommaS -> cont 17#;
	SeparatorT DotS -> cont 18#;
	SeparatorT LAngleBracketS -> cont 19#;
	OperatorT AssignO -> cont 20#;
	OperatorT EqualsO -> cont 21#;
	OperatorT PlusO -> cont 22#;
	OperatorT PlusEqualO -> cont 23#;
	OperatorT GreaterO -> cont 24#;
	OperatorT GreaterEqualO -> cont 25#;
	OperatorT MinusO -> cont 26#;
	OperatorT MinusEqualO -> cont 27#;
	OperatorT LessO -> cont 28#;
	OperatorT LessEqualO -> cont 29#;
	OperatorT TimesO -> cont 30#;
	OperatorT TimesEqualO -> cont 31#;
	OperatorT NotO -> cont 32#;
	OperatorT NotEqualO -> cont 33#;
	OperatorT DivideO -> cont 34#;
	OperatorT DivideEqualO -> cont 35#;
	OperatorT BinaryNotO -> cont 36#;
	OperatorT AndO -> cont 37#;
	OperatorT BinaryAndO -> cont 38#;
	OperatorT BinaryAndEqualO -> cont 39#;
	OperatorT QuestionO -> cont 40#;
	OperatorT OrO -> cont 41#;
	OperatorT BinaryOrO -> cont 42#;
	OperatorT BinaryOrEqualO -> cont 43#;
	OperatorT ColonO -> cont 44#;
	OperatorT PlusPlusO -> cont 45#;
	OperatorT BinaryXorO -> cont 46#;
	OperatorT BinaryXorEqualO -> cont 47#;
	OperatorT MinusMinusO -> cont 48#;
	OperatorT ModulusO -> cont 49#;
	OperatorT ModulusEqualO -> cont 50#;
	OperatorT ShiftLeftO -> cont 51#;
	OperatorT ShiftLeftEqualO -> cont 52#;
	OperatorT ShiftRightO -> cont 53#;
	OperatorT ShiftRightEqualO -> cont 54#;
	OperatorT ShiftRightZeroExtensionO -> cont 55#;
	OperatorT ShiftRightZeroExtensionEqualO -> cont 56#;
	IdentifierT happy_dollar_dollar -> cont 57#;
	LiteralT happy_dollar_dollar -> cont 58#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 59# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Java a -> (a -> Java b) -> Java b
happyThen = (>>=)
happyReturn :: () => a -> Java a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Java a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Java a
happyError' = (\(tokens, _) -> parseError tokens)
parseStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut22 x))

parseExpression tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut23 x))

happySeq = happyDontSeq


parseError :: [Token] -> Java a
parseError []   = issueError $ "Unexpected end of parse."
parseError toks = issueError $ "Parsing error when list of remaining tokens starts with:\n" ++
                               show (take 5 toks)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 18 "<built-in>" #-}
{-# LINE 1 "/Users/rae/local/stow/ghc-8.6.1/lib/ghc-8.6.1/include/ghcversion.h" #-}
















{-# LINE 19 "<built-in>" #-}
{-# LINE 1 "/var/folders/c9/5mf7zds919d2nzdfsz52m_lc0000gp/T/ghc86496_0/ghc_2.h" #-}



































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































{-# LINE 20 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif

{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








{-# LINE 65 "templates/GenericTemplate.hs" #-}


{-# LINE 75 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          

          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)


{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

