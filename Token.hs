-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Token
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Java tokens, for use in communicating between the lexer and the parser
--
----------------------------------------------------------------------------

module Language.Java.Token where
  -- export all definitions

import Language.Java.Literal

--------------------------------------------------------------------------
-- Token definitions
--------------------------------------------------------------------------

data Token
  = IdentifierT String
  | KeywordT Keyword
  | LiteralT Literal
  | SeparatorT Separator
  | OperatorT Operator
  deriving (Show)

data Keyword
  = AbstractK
  | AssertK
  | BooleanK
  | BreakK
  | ByteK
  | CaseK
  | CatchK
  | CharK
  | ClassK
  | ConstK
  | ContinueK
  | DefaultK
  | DoK
  | DoubleK
  | ElseK
  | EnumK
  | ExtendsK
  | FinalK
  | FinallyK
  | FloatK
  | ForK
  | IfK
  | GotoK
  | ImplementsK
  | ImportK
  | InstanceofK
  | IntK
  | InterfaceK
  | LongK
  | NativeK
  | NewK
  | PackageK
  | PrivateK
  | ProtectedK
  | PublicK
  | ReturnK
  | ShortK
  | StaticK
  | StrictfpK
  | SuperK
  | SwitchK
  | SynchronizedK
  | ThisK
  | ThrowK
  | ThrowsK
  | TransientK
  | TryK
  | VoidK
  | VolatileK
  | WhileK
  deriving (Show)

data Separator
  = LParenS
  | RParenS
  | LBraceS
  | RBraceS
  | LBracketS
  | RBracketS
  | SemicolonS
  | CommaS
  | DotS
  | EllipsisS
  | AtS
  | DoubleColonS
  | LAngleBracketS   -- See Note [Parsing "<"] in Lexer.x
  deriving (Show)

data Operator
  = AssignO
  | EqualsO
  | PlusO
  | PlusEqualO
  | GreaterO
  | GreaterEqualO
  | MinusO
  | MinusEqualO
  | LessO
  | LessEqualO
  | TimesO
  | TimesEqualO
  | NotO
  | NotEqualO
  | DivideO
  | DivideEqualO
  | BinaryNotO
  | AndO
  | BinaryAndO
  | BinaryAndEqualO
  | QuestionO
  | OrO
  | BinaryOrO
  | BinaryOrEqualO
  | ColonO
  | PlusPlusO
  | BinaryXorO
  | BinaryXorEqualO
  | ArrowO
  | MinusMinusO
  | ModulusO
  | ModulusEqualO
  | ShiftLeftO
  | ShiftLeftEqualO
  | ShiftRightO
  | ShiftRightEqualO
  | ShiftRightZeroExtensionO
  | ShiftRightZeroExtensionEqualO
  deriving (Show)
