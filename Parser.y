-- -*- haskell -*-
-- The line above tells emacs to use Haskell-style syntax highlighting

{
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

}

%name parseStatement Statement
%name parseExpression Expression

%tokentype { Token }
%error { parseError }

%monad { Java }

%token
   -- keywords
-- TODO  'abstract'              { KeywordT AbstractK }
-- TODO  'assert'                { KeywordT AssertK }
  'boolean'               { KeywordT BooleanK }
-- TODO  'break'                 { KeywordT BreakK }
  'byte'                  { KeywordT ByteK }
-- TODO  'case'                  { KeywordT CaseK }
-- TODO  'catch'                 { KeywordT CatchK }
  'char'                  { KeywordT CharK }
-- TODO  'class'                 { KeywordT ClassK }
-- TODO  'const'                 { KeywordT ConstK }
-- TODO  'continue'              { KeywordT ContinueK }
-- TODO  'default'               { KeywordT DefaultK }
-- TODO  'do'                    { KeywordT DoK }
  'double'                { KeywordT DoubleK }
-- TODO  'else'                  { KeywordT ElseK }
-- TODO  'enum'                  { KeywordT EnumK }
  'extends'               { KeywordT ExtendsK }
-- TODO  'final'                 { KeywordT FinalK }
-- TODO  'finally'               { KeywordT FinallyK }
  'float'                 { KeywordT FloatK }
-- TODO  'for'                   { KeywordT ForK }
-- TODO  'if'                    { KeywordT IfK }
-- TODO  'goto'                  { KeywordT GotoK }
-- TODO  'implements'            { KeywordT ImplementsK }
-- TODO  'import'                { KeywordT ImportK }
  'instanceof'            { KeywordT InstanceofK }
  'int'                   { KeywordT IntK }
-- TODO  'interface'             { KeywordT InterfaceK }
  'long'                  { KeywordT LongK }
-- TODO  'native'                { KeywordT NativeK }
-- TODO  'new'                   { KeywordT NewK }
-- TODO  'package'               { KeywordT PackageK }
-- TODO  'private'               { KeywordT PrivateK }
-- TODO  'protected'             { KeywordT ProtectedK }
-- TODO  'public'                { KeywordT PublicK }
-- TODO  'return'                { KeywordT ReturnK }
  'short'                 { KeywordT ShortK }
-- TODO  'static'                { KeywordT StaticK }
-- TODO  'strictfp'              { KeywordT StrictfpK }
  'super'                 { KeywordT SuperK }
-- TODO  'switch'                { KeywordT SwitchK }
-- TODO  'synchronized'          { KeywordT SynchronizedK }
  'this'                  { KeywordT ThisK }
-- TODO  'throw'                 { KeywordT ThrowK }
-- TODO  'throws'                { KeywordT ThrowsK }
-- TODO  'transient'             { KeywordT TransientK }
-- TODO  'try'                   { KeywordT TryK }
-- TODO  'void'                  { KeywordT VoidK }
-- TODO  'volatile'              { KeywordT VolatileK }
-- TODO  'while'                 { KeywordT WhileK }

   -- separators
  '('                     { SeparatorT LParenS }
  ')'                     { SeparatorT RParenS }
-- TODO  '{'                     { SeparatorT LBraceS }
-- TODO  '}'                     { SeparatorT RBraceS }
  '['                     { SeparatorT LBracketS }
  ']'                     { SeparatorT RBracketS }
-- TODO  ';'                     { SeparatorT SemicolonS }
  ','                     { SeparatorT CommaS }
  '.'                     { SeparatorT DotS }
-- TODO  '...'                   { SeparatorT EllipsisS }
-- TODO  '@'                     { SeparatorT AtS }
-- TODO  '::'                    { SeparatorT DoubleColonS }
  '<'                     { SeparatorT LAngleBracketS }   -- See Note [Parsing "<"] in Lexer.x

   -- operators
   '='                    { OperatorT AssignO }
   '=='                   { OperatorT EqualsO }
   '+'                    { OperatorT PlusO }
   '+='                   { OperatorT PlusEqualO }
   '>'                    { OperatorT GreaterO }
   '>='                   { OperatorT GreaterEqualO }
   '-'                    { OperatorT MinusO }
   '-='                   { OperatorT MinusEqualO }
   ' <'                   { OperatorT LessO }    -- See Note [Parsing "<"] in Lexer.x
   '<='                   { OperatorT LessEqualO }
   '*'                    { OperatorT TimesO }
   '*='                   { OperatorT TimesEqualO }
   '!'                    { OperatorT NotO }
   '!='                   { OperatorT NotEqualO }
   '/'                    { OperatorT DivideO }
   '/='                   { OperatorT DivideEqualO }
   '~'                    { OperatorT BinaryNotO }
   '&&'                   { OperatorT AndO }
   '&'                    { OperatorT BinaryAndO }
   '&='                   { OperatorT BinaryAndEqualO }
   '?'                    { OperatorT QuestionO }
   '||'                   { OperatorT OrO }
   '|'                    { OperatorT BinaryOrO }
   '|='                   { OperatorT BinaryOrEqualO }
   ':'                    { OperatorT ColonO }
   '++'                   { OperatorT PlusPlusO }
   '^'                    { OperatorT BinaryXorO }
   '^='                   { OperatorT BinaryXorEqualO }
-- TODO   '->'                   { OperatorT ArrowO }
   '--'                   { OperatorT MinusMinusO }
   '%'                    { OperatorT ModulusO }
     -- happy thinks %= is a happy command
   'm='                   { OperatorT ModulusEqualO }
   '<<'                   { OperatorT ShiftLeftO }
   '<<='                  { OperatorT ShiftLeftEqualO }
   '>>'                   { OperatorT ShiftRightO }
   '>>='                  { OperatorT ShiftRightEqualO }
   '>>>'                  { OperatorT ShiftRightZeroExtensionO }
   '>>>='                 { OperatorT ShiftRightZeroExtensionEqualO }

    -- identifiers
   IDENTIFIER             { IdentifierT $$ }

    -- literals
   LITERAL                { LiteralT $$ }

-- this ends the definition of tokens, and starts the parsing rules
%%

-------------------------------------------------
-- Identifiers

Identifier :: { Identifier }
Identifier : IDENTIFIER                   { Identifier $1 }
  -- NB: The "value" of an IDENTIFIER is the String that was parsed

------------------------------------------------
-- Literals

Literal :: { Literal }
Literal : LITERAL       { $1 }

------------------------------------------------
-- Types

-- S4.2
PrimitiveType :: { PrimitiveType }
PrimitiveType : NumericType { $1 }
              | 'boolean'   { Boolean }

NumericType :: { PrimitiveType }
NumericType : IntegralType      { $1 }
            | FloatingPointType { $1 }

IntegralType :: { PrimitiveType }
IntegralType : 'byte'    { Byte }
             | 'short'   { Short }
             | 'int'     { Int }
             | 'long'    { Long }
             | 'char'    { Char }

FloatingPointType :: { PrimitiveType }
FloatingPointType : 'float'   { Float }
                  | 'double'  { Double }

-- S4.3
ReferenceType :: { ReferenceType }
ReferenceType : ClassOrInterfaceType        { COIRT $1 }
              | ArrayType                   { ArrayRT $1 }
 -- NB: Including TypeVariable here led to redue/reduce conflicts,
 -- because there is no way to tell the difference between a class
 -- and a variable: String and E look the same to the parser.

-- InterfaceType is the same as ClassType for parsing, so this skips a
-- few steps
ClassOrInterfaceType :: { COIType }
ClassOrInterfaceType : Identifier                            { COIType Nothing $1 [] }
                     | Identifier TypeArguments              { COIType Nothing $1 $2 }
                     | ClassOrInterfaceType '.' Identifier   { COIType (Just $1) $3 [] }
                     | ClassOrInterfaceType '.' Identifier TypeArguments
                                                             { COIType (Just $1) $3 $4 }

ArrayType :: { ArrayType }
ArrayType : PrimitiveType Dims          { PrimArray $1 $2 }
          | ClassOrInterfaceType Dims   { COIArray $1 $2 }

Dims :: { Dims }
Dims : '[' ']'              { Dims 1 }
     | '[' ']' Dims         { case $3 of Dims n -> Dims (n+1) }

-- S4.4
{- TODO (conflict3)
AdditionalBound :: { COIType }
AdditionalBound : '&' ClassOrInterfaceType  { $2 }

AdditionalBounds :: { [COIType] }
AdditionalBounds : {- empty -}                      { [] }
                 | AdditionalBound AdditionalBounds { $1 : $2 }
-}

-- S4.5.1
TypeArguments :: { [TypeArgument] }
TypeArguments : '<' TypeArgumentList '>'     { $2 }

TypeArgumentList :: { [TypeArgument] }
TypeArgumentList : TypeArgument                      { [$1] }
                 | TypeArgument ',' TypeArgumentList { $1 : $3 }

TypeArgument :: { TypeArgument }
TypeArgument : ReferenceType          { ReferenceArgument $1 }
             | Wildcard               { WildcardArgument $1 }

Wildcard :: { Wildcard }
Wildcard : '?'                  { Wildcard Nothing }
         | '?' WildcardBounds   { Wildcard (Just $2) }

WildcardBounds :: { WildcardBounds }
WildcardBounds : 'extends' ReferenceType    { ExtendsWB $2 }
               | 'super' ReferenceType      { SuperWB $2 }

------------------------------------------------
-- Names

-- S6.5

-- See comments around declaration of QualifiedName for why this deviates from
-- the standard
QualifiedName :: { QualifiedName }
QualifiedName : ReversedDottedIdentifiersNE         { case $1 of id :| quals -> QualifiedName (reverse quals) id }

ReversedDottedIdentifiersNE :: { NonEmpty Identifier }  -- the resulting list is in *reverse* order
ReversedDottedIdentifiersNE : Identifier                                 { $1 :| [] }
                            | ReversedDottedIdentifiersNE '.' Identifier { $3 `cons` $1 }


------------------------------------------------
-- Statements

Statement :: { Statement }
Statement : Expression    { ExpressionStatement $1 }

------------------------------------------------
-- Expressions

-- S15.2
Expression :: { Expression }
Expression : AssignmentExpression         { $1 }
-- TODO (conflict2)          | LambdaExpression             { $1 }


-- S15.8
Primary :: { Expression }
Primary : PrimaryNoNewArray       { $1 }
-- TODO        | ArrayCreationExpression { $1 }

PrimaryNoNewArray :: { Expression }
PrimaryNoNewArray : Literal                   { LiteralE $1 }
-- TODO (conflict1)                  | ClassLiteral              { ClassLiteralE $1 }
                  | 'this'                    { ThisE NoTQ }
-- TODO (conflict1)                  | QualifiedName '.' 'this'  { ThisE (TypeNameTQ $1) }
                  | '(' Expression ')'        { $2 }
-- TODO                  | ClassInstanceCreationExpression { ClassInstanceCreationE }
                  | FieldAccess               { FieldAccessE $1 }
                  | ArrayAccess               { ArrayAccessE $1 }
-- TODO                  | MethodInvocation          { $1 }
-- TODO                  | MethodReference           { $1 }

{- TODO (conflict1)
-- S15.8.2
ClassLiteral :: { ClassLiteral }
ClassLiteral : QualifiedName '.' 'class'            { TypeNameCL $1 Nothing }
             | QualifiedName Dims '.' 'class'       { TypeNameCL $1 (Just $2) }
             | PrimitiveType '.' 'class'            { PrimitiveTypeCL $1 Nothing }
             | PrimitiveType Dims '.' 'class'       { PrimitiveTypeCL $1 (Just $2) }
             | 'void' '.' 'class'                   { VoidCL }
-}

-- S15.10.3
ArrayAccess :: { ArrayAccess }
ArrayAccess : QualifiedName '[' Expression ']'      { ArrayAccess (NameE $1) $3 }
            | PrimaryNoNewArray '[' Expression ']'  { ArrayAccess $1 $3 }

-- S15.11
FieldAccess :: { FieldAccess }
FieldAccess : Primary '.' Identifier                   { FieldAccess (ExpressionFAQ $1) $3 }
            | 'super' '.' Identifier                   { FieldAccess (SuperFAQ NoTQ) $3 }
-- TODO (conflict1)            | QualifiedName '.' 'super' '.' Identifier { FieldAccess (SuperFAQ (TypeNameTQ $1)) $5 }

-- S15.14
PostfixExpression :: { Expression }
PostfixExpression : Primary                    { $1 }
                  | QualifiedName              { NameE $1 }
                  | PostIncrementExpression    { $1 }
                  | PostDecrementExpression    { $1 }

-- S15.14.2
PostIncrementExpression :: { Expression }
PostIncrementExpression : PostfixExpression '++'   { UnaryE PostIncrement $1 }

-- S15.14.3
PostDecrementExpression :: { Expression }
PostDecrementExpression : PostfixExpression '--'   { UnaryE PostDecrement $1 }

-- S15.15
UnaryExpression :: { Expression }
UnaryExpression : PreIncrementExpression      { $1 }
                | PreDecrementExpression      { $1 }
                | '+' UnaryExpression         { UnaryE UnaryPlus $2 }
                | '-' UnaryExpression         { UnaryE Negation $2 }
                | UnaryExpressionNotPlusMinus { $1 }

PreIncrementExpression :: { Expression }
PreIncrementExpression : '++' UnaryExpression { UnaryE PreIncrement $2 }

PreDecrementExpression :: { Expression }
PreDecrementExpression : '--' UnaryExpression { UnaryE PreDecrement $2 }

UnaryExpressionNotPlusMinus :: { Expression }
UnaryExpressionNotPlusMinus : PostfixExpression   { $1 }
                            | '~' UnaryExpression { UnaryE BitwiseNot $2 }
                            | '!' UnaryExpression { UnaryE LogicalNot $2 }
                            | CastExpression      { $1 }

-- S15.16
CastExpression :: { Expression }
CastExpression : '(' PrimitiveType ')' UnaryExpression { CastE (PrimCastType $2) $4 }
-- TODO (conflict3)               | '(' ReferenceType AdditionalBounds ')' UnaryExpressionNotPlusMinus
-- TODO (conflict3)                                                       { CastE (RefCastType $2 $3) $5 }
-- TODO (conflict2)               | '(' ReferenceType AdditionalBounds ')' LambdaExpression
-- TODO (conflict2)                                                       { CastE (RefCastType $2 $3) $5 }

-- S15.17
MultiplicativeExpression :: { Expression }
MultiplicativeExpression : UnaryExpression    { $1 }
                         | MultiplicativeExpression '*' UnaryExpression
                                              { BinaryE $1 Times $3 }
                         | MultiplicativeExpression '/' UnaryExpression
                                              { BinaryE $1 DividedBy $3 }
                         | MultiplicativeExpression '%' UnaryExpression
                                              { BinaryE $1 Modulus $3 }

-- S15.18
AdditiveExpression :: { Expression }
AdditiveExpression : MultiplicativeExpression       { $1 }
                   | AdditiveExpression '+' MultiplicativeExpression
                                                    { BinaryE $1 Plus $3 }
                   | AdditiveExpression '-' MultiplicativeExpression
                                                    { BinaryE $1 Minus $3 }

-- S15.19
ShiftExpression :: { Expression }
ShiftExpression : AdditiveExpression                       { $1 }
                | ShiftExpression '<<' AdditiveExpression  { BinaryE $1 LeftShift $3 }
                | ShiftExpression '>>' AdditiveExpression  { BinaryE $1 RightShift $3 }
                | ShiftExpression '>>>' AdditiveExpression { BinaryE $1 RightShiftZeroExtension $3 }

-- S15.20
RelationalExpression :: { Expression }
RelationalExpression : ShiftExpression      { $1 }
                        -- NB: ' <', not '<' in this line. See Note [Parsing "<"] in Lexer.x
                     | RelationalExpression ' <' ShiftExpression
                                            { BinaryE $1 LessThan $3 }
                     | RelationalExpression '>' ShiftExpression
                                            { BinaryE $1 GreaterThan $3 }
                     | RelationalExpression '<=' ShiftExpression
                                            { BinaryE $1 LessThanEquals $3 }
                     | RelationalExpression '>=' ShiftExpression
                                            { BinaryE $1 GreaterThanEquals $3 }
                     | RelationalExpression 'instanceof' ReferenceType
                                            { InstanceofE $1 $3 }

-- S15.21
EqualityExpression :: { Expression }
EqualityExpression : RelationalExpression        { $1 }
                   | EqualityExpression '==' RelationalExpression
                                                 { BinaryE $1 Equality $3 }
                   | EqualityExpression '!=' RelationalExpression
                                                 { BinaryE $1 Disequality $3 }

-- S15.22
AndExpression :: { Expression }
AndExpression : EqualityExpression                      { $1 }
              | AndExpression '&' EqualityExpression    { BinaryE $1 BitwiseAnd $3 }

ExclusiveOrExpression :: { Expression }
ExclusiveOrExpression : AndExpression                             { $1 }
                      | ExclusiveOrExpression '^' AndExpression   { BinaryE $1 BitwiseExclusiveOr $3 }

InclusiveOrExpression :: { Expression }
InclusiveOrExpression : ExclusiveOrExpression    { $1 }
                      | InclusiveOrExpression '|' ExclusiveOrExpression
                                                 { BinaryE $1 BitwiseOr $3 }

-- S15.23
ConditionalAndExpression :: { Expression }
ConditionalAndExpression : InclusiveOrExpression    { $1 }
                         | ConditionalAndExpression '&&' InclusiveOrExpression
                                                    { BinaryE $1 LogicalAnd $3 }

-- S15.24
ConditionalOrExpression :: { Expression }
ConditionalOrExpression : ConditionalAndExpression  { $1 }
                        | ConditionalOrExpression '||' ConditionalAndExpression
                                                    { BinaryE $1 LogicalOr $3 }

-- S15.26
ConditionalExpression :: { Expression }
ConditionalExpression : ConditionalOrExpression    { $1 }
                      | ConditionalOrExpression '?' Expression ':' ConditionalExpression
                                                   { ConditionalE $1 $3 $5 }
-- TODO (conflict2)                     | ConditionalOrExpression '?' Expression ':' LambdaExpression
-- TODO (conflict2)                                                  { ConditionalE $1 $3 $5 }

-- S15.26
AssignmentExpression :: { Expression }
AssignmentExpression : ConditionalExpression  { $1 }
                     | Assignment             { $1 }

Assignment :: { Expression }
Assignment : LeftHandSide AssignmentOperator Expression  { AssignmentE $1 $2 $3 }

LeftHandSide :: { LeftHandSide }
LeftHandSide : QualifiedName     { ExpressionNameLHS $1 }
             | FieldAccess       { FieldAccessLHS $1 }
             | ArrayAccess       { ArrayAccessLHS $1 }

AssignmentOperator :: { AssignmentOperator }
AssignmentOperator : '='    { Assigns }
                   | '*='   { TimesAssigns }
                   | '/='   { DividedByAssigns }
                   | 'm='   { ModulusAssigns }
                   | '+='   { PlusAssigns }
                   | '-='   { MinusAssigns }
                   | '<<='  { ShiftLeftAssigns }
                   | '>>='  { ShiftRightAssigns }
                   | '>>>=' { ShiftRightZeroExtensionAssigns }
                   | '&='   { BitwiseAndAssigns }
                   | '^='   { BitwiseExclusiveOrAssigns }
                   | '|='   { BitwiseOrAssigns }

{- TODO (conflict2)

-- S15.27
LambdaExpression :: { Expression }
LambdaExpression : LambdaParameters '->' LambdaBody   { LambdaE $1 $3 }

-- S15.27.1
LambdaParameters :: { LambdaParameters }
LambdaParameters : Identifier                   { IdentifierLP $1 }
                 | '(' ')'                      { FormalParameterListLP [] }
-- TODO                 | '(' FormalParameterList ')'  { FormalParameterListLP $2 }
                 | '(' Identifiers1 ')'         { InferredFormalParameterListLP $2 }

-- A comma-separated list containing at least one element
Identifiers1 :: { [Identifier] }
Identifiers1 : Identifier                     { [$1] }
             | Identifier ',' Identifiers1    { $1 : $3 }

LambdaBody :: { LambdaBody }
LambdaBody : Expression       { ExpressionLB $1 }
-- TODO           | Block            { BlockLB $1 }
-}

-- more Haskell code tht's part of the parser
{
parseError :: [Token] -> Java a
parseError []   = issueError $ "Unexpected end of parse."
parseError toks = issueError $ "Parsing error when list of remaining tokens starts with:\n" ++
                               show (take 5 toks)
}
