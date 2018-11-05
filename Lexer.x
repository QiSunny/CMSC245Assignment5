-- -*- haskell -*-
-- The line above tells emacs to use Haskell-style syntax highlighting

{
{-# OPTIONS_GHC -Wno-unused-imports #-}   -- alex includes unnecessary imports

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Lexer
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- A Java lexer, based on Chapter 3 of the Java Language Specification
--
----------------------------------------------------------------------------

{-
Note [Parsing "<"]
~~~~~~~~~~~~~~~~~~
When Java was originally designed, there were no type parameters. For example,
there was no ArrayList<Integer>, there was just ArrayList. All containers
stored Objects, and you were required to cast every time you retrieved an
element from a container. Life was bad.

However, when generics were introduced in Java 5, the new syntax, using
< and >, conflicted with existing usages of < and > (as comparison operators).
For example, does "foo<bar.baz" start an expression, comparing foo against
bar.baz? Or is it the beginning of a parameterized type foo, where bar.baz
is the parameter? It's impossible to know until we find the >, which can
be arbitrarily far away. Real Java parsers go through machinations to get
around this ambiguity, but we don't want the parser here to be quite so
difficult. Instead, we use the following rules:

  Iff "<" is preceded by whitespace, it is a comparison operator.
  Iff "<" is preceded by non-whitespace, it begins a type argument.

This is a bit silly to use whitespace in this way, but it's very convenient.

According to the documentation for Alex, each of the lexing rules tries
to consume as much of the input as possible. So, even though the rules
for `@WhiteSpace` and `@WhiteSpace "<"` overlap, the second one will always
win. Along similar lines, the rule for plain `"<"` will never fire if the
symbol is preceded by whitespace. We can thus detect these two cases easily.

We must similarly define extra rules for <=, <<, and <<=, because otherwise
the " <" that might precede them might get mistaken for a LessThan.

Naturally, the two meanings of < corresond to different tokens.

Interestingly, there is no such challenge with >, because we know whether
or not we've recently passed a < that is that beginning of a type argument.
However (and unlike real Java implementations), we fail on something like
`ArrayList<List<Bool>>`, because the `>>` at the end is lexed as a right-
shift operator. It wouldn't be hard to fix this, actually: we could refuse,
for example, to lex a right-shift after the beginning of a type argument
(until we see the end of the type argument). Alex supports stateful lexing,
so this should be doable without terribly much fuss. One challenge is that
it would have to track the number of type-argument beginnings so it could
be sure when the type argument is truly over. Though Alex can be stateful,
it doesn't internally support an unbounded number of states. The solution
would be to store the type-argument depth in a monad and use an Alex
predicate, but that's way more elaborate than I want here.
-}

module Language.Java.Lexer ( lexJava, lexJavaEither ) where

import Data.Char ( digitToInt, isDigit )
import qualified Data.Map as M

import Language.Java.Panic
import Language.Java.Token
import Language.Java.Literal
import Language.Java.Monad
import Language.Java.Char

}

-- This imports various automatic processing utilities. See the alex manual.
%wrapper "monadUserState"

$InputCharacter = . # [\r]
$AnyChar = [.\n]

$JavaLetter = [A-Za-z_\$]     -- S3.8
$JavaLetterOrDigit = [0-9$JavaLetter]

@LineTerminator = [\r\n] | \r\n                   -- newlines (S3.4)
@WhiteSpace     = \                               -- whitespace (S3.6)
                | \t
                | \f
                | @LineTerminator

-- Comments, S3.7
@CommentElement    = ($AnyChar # \*) | \*+ ($AnyChar # \/)
@CommentTerminator = \*+ \/
@CommentTail       = @CommentElement* @CommentTerminator

@TraditionalComment = "/*" @CommentTail

@EndOfLineComment   = "//" $InputCharacter*

@Comment        =  @TraditionalComment
                |  @EndOfLineComment


-- Integer literals, S3.10.1
$IntegerTypeSuffix = [lL]
$Digit             = 0-9
$NonZeroDigit      = 1-9
@DecimalNumeral    = 0
                   | $NonZeroDigit
                   | $NonZeroDigit ($Digit | _)* $Digit


$HexDigit   = [0-9a-fA-F]
@HexDigits  = $HexDigit
            | $HexDigit ($HexDigit | _)* $HexDigit
@HexNumeral = 0 [xX] @HexDigits


$OctalDigit   = [0-7]
@OctalNumeral = 0 ($OctalDigit | _)* $OctalDigit

$BinaryDigit   = [01]
@BinaryDigits  = $BinaryDigit
               | $BinaryDigit ($BinaryDigit | _)* $BinaryDigit
@BinaryNumeral = 0 [bB] @BinaryDigits

-- Floating point literals, S3.10.2
@Digits          = $Digit
                 | $Digit ($Digit | _)* $Digit

$FloatTypeSuffix = [fFdD]
@SignedInteger   = [\+\-]? @Digits
@ExponentPart    = [eE] @SignedInteger

@DecimalFloatingPointLiteral = @Digits \. @Digits? @ExponentPart? $FloatTypeSuffix?
                             | \. @Digits @ExponentPart? $FloatTypeSuffix?
                             | @Digits @ExponentPart $FloatTypeSuffix?
                             | @Digits @ExponentPart? $FloatTypeSuffix

@HexSignificand                  = @HexNumeral \.?
                                 | 0 [xX] @HexDigits? \. @HexDigits
@BinaryExponent                  = [pP] @SignedInteger
@HexadecimalFloatingPointLiteral = @HexSignificand @BinaryExponent $FloatTypeSuffix?

-- Escape sequences, S3.10.6
@EscapeSequence = \\ ( [btnfr\"\'\\]
                     | $OctalDigit
                     | $OctalDigit $OctalDigit
                     | [0-3] $OctalDigit $OctalDigit )

-- Character literals, S3.10.4
$SingleCharacter  = $InputCharacter # ['\\]
@CharacterLiteral = ' $SingleCharacter '
                  | ' @EscapeSequence '


-- String literals, S3.10.5
@StringCharacter = $InputCharacter # [\"\\]
                 | @EscapeSequence

@StringLiteral = \" @StringCharacter* \"

-- This line tells alex that the actual lexing patterns come next.
-- The word before the ":-" is completely ignored.
java :-

-- Ignore comments and whitespace
@Comment                            ;
@WhiteSpace                         ;

-- Identifiers, S3.8
$JavaLetter $JavaLetterOrDigit*     { tokenS identifier }

-- Integer literals, S3.10.1
-- IRREG: Despite the JLS's exhortation, we do *not* fail on overflowing literals.
@DecimalNumeral                     { tokenS (LiteralT . IntL . read . stripUnderscores) }
@DecimalNumeral $IntegerTypeSuffix  { tokenS (LiteralT . LongL . read . stripUnderscores . init) }

@HexNumeral                         { tokenS (LiteralT . IntL . read . stripUnderscores) }
@HexNumeral $IntegerTypeSuffix      { tokenS (LiteralT . LongL . read . stripUnderscores . init) }

@OctalNumeral                       { tokenS (LiteralT . IntL . read . insertOctalO . stripUnderscores) }
@OctalNumeral $IntegerTypeSuffix    { tokenS (LiteralT . LongL . read . insertOctalO . stripUnderscores . init) }

@BinaryNumeral                      { tokenS (LiteralT . IntL . readBin . stripUnderscores) }
@BinaryNumeral $IntegerTypeSuffix   { tokenS (LiteralT . LongL . readBin . stripUnderscores . init) }

@DecimalFloatingPointLiteral        { tokenS lexFloatLiteral }
@HexadecimalFloatingPointLiteral    { tokenS (\s -> unimplemented $ "lex hexademical literals like " ++ s) }

@CharacterLiteral                   { tokenS (LiteralT . CharL . charHtoJ . read . insertOctalOEscapes) }

@StringLiteral                      { tokenS (LiteralT . StringL . stringHtoJ . read . insertOctalOEscapes) }

-- Separators, S3.11
"("     { sep LParenS }
")"     { sep RParenS }
"{"     { sep LBraceS }
"}"     { sep RBraceS }
"["     { sep LBracketS }
"]"     { sep RBracketS }
";"     { sep SemicolonS }
","     { sep CommaS }
"."     { sep DotS }
"..."   { sep EllipsisS }
"@"     { sep AtS }
"::"    { sep DoubleColonS }
"<"     { sep LAngleBracketS }    -- See Note [Parsing "<"]

-- Operators, S3.12
"="    { op AssignO }
"=="   { op EqualsO }
"+"    { op PlusO }
"+="   { op PlusEqualO }
">"    { op GreaterO }
">="   { op GreaterEqualO }
"-"    { op MinusO }
"-="   { op MinusEqualO }
@WhiteSpace "<"    { op LessO }   -- See Note [Parsing "<"]
"<="   { op LessEqualO }
@WhiteSpace "<="   { op LessEqualO } -- See Note [Parsing "<"]
"*"    { op TimesO }
"*="   { op TimesEqualO }
"!"    { op NotO }
"!="   { op NotEqualO }
"/"    { op DivideO }
"/="   { op DivideEqualO }
"~"    { op BinaryNotO }
"&&"   { op AndO }
"&"    { op BinaryAndO }
"&="   { op BinaryAndEqualO }
"?"    { op QuestionO }
"||"   { op OrO }
"|"    { op BinaryOrO }
"|="   { op BinaryOrEqualO }
":"    { op ColonO }
"++"   { op PlusPlusO }
"^"    { op BinaryXorO }
"^="   { op BinaryXorEqualO }
"->"   { op ArrowO }
"--"   { op MinusMinusO }
"%"    { op ModulusO }
"%="   { op ModulusEqualO }
"<<"   { op ShiftLeftO }
@WhiteSpace "<<"  { op ShiftLeftO } -- See Note [Parsing "<"]
"<<="  { op ShiftLeftEqualO }
@WhiteSpace "<<=" { op ShiftLeftEqualO } -- See Note [Parsing "<"]
">>"   { op ShiftRightO }
">>="  { op ShiftRightEqualO }
">>>"  { op ShiftRightZeroExtensionO }
">>>=" { op ShiftRightZeroExtensionEqualO }

-- This brace marks the start of Haskell code that will process the above
-- directives
{

--------------------------------------------------------------------------
-- This region is all internal Alex plumbing. Haskell experts only!
--------------------------------------------------------------------------

type AlexUserState = [Token]   -- in reverse order for easy consing

alexUpdUserState :: ([Token] -> [Token]) -> Alex ()
alexUpdUserState upd = do
  st <- alexGetUserState
  alexSetUserState (upd st)

alexGetString :: AlexInput -> Int -> String
alexGetString (_,_,_,str) len = take len str

tokenS :: (String -> Token) -> AlexInput -> Int -> Alex ()
tokenS f input len = do
  let tok = f (alexGetString input len)
  alexUpdUserState (tok :)
  alexMonadScan  -- and continue

-- This is called when the end of file is reached
alexEOF :: Alex ()
alexEOF = do
  (AlexPn _ line column, _, remaining_bytes, remaining_chars) <- alexGetInput
  if null remaining_bytes && null remaining_chars
    then return () -- success
    else alexError $ "Unexpected end of file at line " ++ show line ++ ", column " ++ show column

-- the initial state
alexInitUserState :: [Token]
alexInitUserState = []

--------------------------------------------------------------------------
-- Utility functions used during lexing
--------------------------------------------------------------------------

-- Convert a String into a Token, recognizing keywords.
identifier :: String -> Token
 -- the literals are easy enough to handle by direct pattern matching
identifier "true"  = LiteralT (BooleanL True)
identifier "false" = LiteralT (BooleanL False)
identifier "null"  = LiteralT NullL
identifier str
  = case M.lookup str keywordMap of
      Nothing -> IdentifierT str -- not a keyword
      Just k  -> KeywordT k      -- a keyword

-- See S3.9
keywordMap :: M.Map String Keyword
keywordMap = M.fromList
  [ ("abstract",     AbstractK)
  , ("assert",       AssertK)
  , ("boolean",      BooleanK)
  , ("break",        BreakK)
  , ("byte",         ByteK)
  , ("case",         CaseK)
  , ("catch",        CatchK)
  , ("char",         CharK)
  , ("class",        ClassK)
  , ("const",        ConstK)
  , ("continue",     ContinueK)
  , ("default",      DefaultK)
  , ("do",           DoK)
  , ("double",       DoubleK)
  , ("else",         ElseK)
  , ("enum",         EnumK)
  , ("extends",      ExtendsK)
  , ("final",        FinalK)
  , ("finally",      FinallyK)
  , ("float",        FloatK)
  , ("for",          ForK)
  , ("if",           IfK)
  , ("goto",         GotoK)
  , ("implements",   ImplementsK)
  , ("import",       ImportK)
  , ("instanceof",   InstanceofK)
  , ("int",          IntK)
  , ("interface",    InterfaceK)
  , ("long",         LongK)
  , ("native",       NativeK)
  , ("new",          NewK)
  , ("package",      PackageK)
  , ("private",      PrivateK)
  , ("protected",    ProtectedK)
  , ("public",       PublicK)
  , ("return",       ReturnK)
  , ("short",        ShortK)
  , ("static",       StaticK)
  , ("strictfp",     StrictfpK)
  , ("super",        SuperK)
  , ("switch",       SwitchK)
  , ("synchronized", SynchronizedK)
  , ("this",         ThisK)
  , ("throw",        ThrowK)
  , ("throws",       ThrowsK)
  , ("transient",    TransientK)
  , ("try",          TryK)
  , ("void",         VoidK)
  , ("volatile",     VolatileK)
  , ("while",        WhileK)
  ]

-- remove all underscores from a string
stripUnderscores :: String -> String
stripUnderscores = filter (/= '_')

-- insert an "o" as the second character, so that 'read' interprets as an octal number
-- precondition: input has at least two characters
insertOctalO :: String -> String
insertOctalO (zero : rest) = zero : 'o' : rest
insertOctalO []            = panic "Zero-character octal literal"

-- convert a binary literal into a number; there is nothing built-in that does this
readBin :: Integral a => String -> a
readBin (_zero : _b : digs) = go 0 (map digitToInt digs)
  where
    go :: Integral a => a -> [Int] -> a
    go acc []       = acc
    go acc (b : bs) = go (acc * 2 + fromIntegral b) bs
readBin other = panic $ "Zero- or one-character binary literal: " ++ other

-- understand a floating point literal; IRREG: no guarantees that this implements
-- exactly the IEEE standard
lexFloatLiteral :: String -> Token
lexFloatLiteral str = LiteralT $ go1 0 (stripUnderscores str)
  where
    -- look for the integer part, before the decimal point (if any)
    go1 :: Integer -> String -> Literal
    go1 acc []                   = DoubleL (fromInteger acc) -- this shouldn't happen (no suffix)
    go1 acc ('.' : chars)        = go2 (fromInteger acc) 0.1 chars
    go1 acc (e : chars) | is_e e = go3 (fromInteger acc) chars
    go1 acc [f]         | is_f f = FloatL (fromInteger acc)
    go1 acc [d]         | is_d d = DoubleL (fromInteger acc)
    go1 acc (num : chars)        = go1 (acc * 10 + toInteger (digitToInt num)) chars

    -- look for the part after the decimal
    go2 :: Rational   -- what we have so far
        -> Rational   -- multiplier for next digit
        -> String
        -> Literal
    go2 acc _mult []                   = DoubleL (fromRational acc)
    go2 acc _mult (e : chars) | is_e e = go3 acc chars
    go2 acc _mult [f]         | is_f f = FloatL (fromRational acc)
    go2 acc _mult [d]         | is_d d = DoubleL (fromRational acc)
    go2 acc mult  (num : chars)        = go2 (acc + mult * fromIntegral (digitToInt num)) (mult / 10) chars

    -- look for the part after the exponent
    go3 :: Rational  -- what we have so far
        -> String
        -> Literal
    go3 acc ('+' : chars) = go4 acc True  0 chars
    go3 acc ('-' : chars) = go4 acc False 0 chars
    go3 acc chars         = go4 acc True  0 chars

    -- look for the part after the exponent sign
    go4 :: Rational -- what we have so far
        -> Bool     -- is the exponent positive?
        -> Integer  -- part of the exponent
        -> String
        -> Literal
    go4 acc pos exp_acc []            = DoubleL (finish acc pos exp_acc)
    go4 acc pos exp_acc [f] | is_f f  = FloatL (finish acc pos exp_acc)
    go4 acc pos exp_acc [d] | is_d d  = DoubleL (finish acc pos exp_acc)
    go4 acc pos exp_acc (num : chars) = go4 acc pos (exp_acc * 10 + toInteger (digitToInt num)) chars

    finish :: Fractional a
           => Rational  -- base number
           -> Bool      -- is the exponent positive?
           -> Integer   -- exponent
           -> a         -- computed value
    finish acc pos exp_acc = fromRational (acc * (10 ^^ maybe_neg pos exp_acc))

     -- if the Bool is False, negate the number
    maybe_neg :: Bool -> Integer -> Integer
    maybe_neg pos num | pos       = num
                      | otherwise = -num

    is_f 'f' = True
    is_f 'F' = True
    is_f _   = False

    is_d 'd' = True
    is_d 'D' = True
    is_d _   = False

    is_e 'e' = True
    is_e 'E' = True
    is_e _   = False

-- accept a separator
sep :: Separator -> AlexAction ()
sep s = tokenS (\_ -> SeparatorT s)

-- accept an operator
op :: Operator -> AlexAction ()
op o = tokenS (\_ -> OperatorT o)

--------------------------------------------------------------------------
-- The main entry point into this file
--------------------------------------------------------------------------

-- | Lex some Java code, issuing an error in the Java monad if there is
-- a lexical error.
lexJava :: String -> Java [Token]
lexJava = eitherToJava . lexJavaEither

-- | Lex Java. Returns either an error string (suitable to be reported to
-- a user) or the list of tokens in the file.
lexJavaEither :: String -> Either String [Token]
lexJavaEither input = runAlex input (alexMonadScan >> (fmap reverse alexGetUserState))
}
