-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Syntax
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines a Java AST, heavily based on definition in the Java Language
-- Specification for Java 8.
--
----------------------------------------------------------------------------

module Language.Java.Syntax where

import Language.Java.Literal
import Language.Java.Identifier
import Language.Java.Type

import Data.List.NonEmpty

---------------------------------------------
-- Expressions

data Expression
  = NameE QualifiedName
  | LambdaE LambdaParameters LambdaBody
  | ConditionalE Expression Expression Expression
  | AssignmentE LeftHandSide AssignmentOperator Expression
  | BinaryE Expression BinaryOperator Expression
  | InstanceofE Expression ReferenceType
  | UnaryE UnaryOperator Expression
  | CastE CastType Expression
  | LiteralE Literal
  | ClassLiteralE ClassLiteral
  | ThisE TypeQualifier
  | ClassInstanceCreationE  -- TODO
  | ArrayCreationE          -- TODO
  | FieldAccessE FieldAccess
  | ArrayAccessE ArrayAccess
  | MethodInvocationE MethodInvocationQualifier [TypeArgument] Identifier [Expression]
  | MethodReferenceE MethodReferenceQualifier [TypeArgument] MethodReferenceMethod
  deriving (Show)

data ClassLiteral
  = TypeNameCL QualifiedName (Maybe Dims)
  | PrimitiveTypeCL PrimitiveType (Maybe Dims)
  | VoidCL
  deriving (Show)

data TypeQualifier
  = TypeNameTQ QualifiedName
  | NoTQ
  deriving (Show)

data LambdaParameters
  = IdentifierLP Identifier
  | FormalParameterListLP [FormalParameter]
  | InferredFormalParameterListLP [Identifier]
  deriving (Show)

data LambdaBody
  = ExpressionLB Expression
  | BlockLB Block
  deriving (Show)

data LeftHandSide
  = ExpressionNameLHS QualifiedName
  | FieldAccessLHS FieldAccess
  | ArrayAccessLHS ArrayAccess
  deriving (Show)

data ArrayAccess
  = ArrayAccess Expression Expression
  deriving (Show)

data MethodInvocationQualifier
  = NoMIQ
  | TypeNameMIQ QualifiedName
  | ExpressionMIQ Expression
  | SuperMIQ TypeQualifier
  deriving (Show)

data MethodReferenceQualifier
  = ExpressionMRQ Expression
  | ReferenceTypeMRQ ReferenceType
  | SuperMRQ TypeQualifier
  deriving (Show)

data MethodReferenceMethod
  = IdentifierMRM Identifier
  | NewMRM
  deriving (Show)

data FieldAccess
  = FieldAccess FieldAccessQualifier Identifier
  deriving (Show)

data FieldAccessQualifier
  = ExpressionFAQ Expression
  | SuperFAQ TypeQualifier
  deriving (Show)

data AssignmentOperator
  = Assigns
  | TimesAssigns
  | DividedByAssigns
  | ModulusAssigns
  | PlusAssigns
  | MinusAssigns
  | ShiftLeftAssigns
  | ShiftRightAssigns
  | ShiftRightZeroExtensionAssigns
  | BitwiseAndAssigns
  | BitwiseExclusiveOrAssigns
  | BitwiseOrAssigns
  deriving (Show)

data BinaryOperator
  = LogicalOr
  | LogicalAnd
  | BitwiseOr
  | BitwiseExclusiveOr
  | BitwiseAnd
  | Equality
  | Disequality
  | LessThan
  | GreaterThan
  | LessThanEquals
  | GreaterThanEquals
  | LeftShift
  | RightShift
  | RightShiftZeroExtension
  | Plus
  | Minus
  | Times
  | DividedBy
  | Modulus
  deriving (Eq, Show)

data UnaryOperator
  = PreIncrement
  | PreDecrement
  | UnaryPlus
  | Negation
  | BitwiseNot
  | LogicalNot
  | PostIncrement
  | PostDecrement
  deriving (Show)

data CastType
  = PrimCastType PrimitiveType
  | RefCastType ReferenceType [COIType]  -- the list is the additional bounds, if any
  deriving (Show)

-------------------------------------------
-- Blocks

data Block
  = Block [BlockStatement]
  deriving (Show)

data BlockStatement
  = LocalVariableDeclarationStatement LocalVariableDeclaration
  | ClassDeclarationStatement ClassDeclaration
  | StatementBlock Statement
  deriving (Show)

data LocalVariableDeclaration
  = LocalVariableDeclaration [VariableModifier] Type [VariableDeclarator]
  deriving (Show)

data VariableModifier
  = FinalVM
  deriving (Show)

data VariableDeclarator
  = VariableDeclarator VariableDeclaratorId (Maybe VariableInitializer)
  deriving (Show)

data VariableDeclaratorId
  = VariableDeclaratorId Identifier (Maybe Dims)
  deriving (Show)

data VariableInitializer
  = ExpressionVI Expression
  | ArrayInitializerVI ArrayInitializer
  deriving (Show)

data ArrayInitializer
  = ArrayInitializer [VariableInitializer]
  deriving (Show)

-----------------------------------------
-- Classes

data ClassDeclaration
  = ClassDeclaration [ClassModifier] Identifier [TypeParameter] (Maybe ClassType) [InterfaceType] ClassBody
  deriving (Show)

data ClassModifier
  = PublicCM
  | ProtectedCM
  | PrivateCM
  | AbstractCM
  | StaticCM
  | FinalCM
  | StrictfpCM
  deriving (Show)

data ClassBody
  = ClassBody [ClassBodyDeclaration]
  deriving (Show)

data ClassBodyDeclaration
  = MemberDeclaration ClassMemberDeclaration
  | InstanceInitializer Block
  | StaticInitializer Block
  | ConstructorDeclaration [ConstructorModifier] ConstructorDeclarator (Maybe Throws) ConstructorBody
  deriving (Show)

data ClassMemberDeclaration
  = FieldCMD [FieldModifier] Type [VariableDeclarator]
  | MethodCMD [MethodModifier] MethodHeader MethodBody
  | ClassCMD ClassDeclaration
  | InterfaceCMD InterfaceDeclaration
  | EmptyCMD
  deriving (Show)

data InterfaceDeclaration
  = InterfaceDeclaration [InterfaceModifier] Identifier [TypeParameter] [InterfaceType] InterfaceBody
  deriving (Show)

data InterfaceModifier
  = PublicIM
  | ProtectedIM
  | PrivateIM
  | AbstractIM
  | StaticIM
  | StrictfpIM
  deriving (Show)

data InterfaceBody
  = InterfaceBody [InterfaceMemberDeclaration]
  deriving (Show)

data InterfaceMemberDeclaration
  = ConstantIMD ConstantDeclaration
  | MethodIMD [InterfaceMethodModifier] MethodHeader MethodBody
  | ClassIMD ClassDeclaration
  | InterfaceIMD InterfaceDeclaration
  | EmptyIMD
  deriving (Show)

data ConstantDeclaration
  = ConstantDeclaration [ConstantModifier] Type [VariableDeclarator]
  deriving (Show)

data ConstantModifier
  = PublicConstM
  | StaticConstM
  | FinalConstM
  deriving (Show)

data InterfaceMethodModifier
  = PublicIMM
  | AbstractIMM
  | DefaultIMM
  | StaticIMM
  | StrictfpIMM
  deriving (Show)

data FieldModifier
  = PublicFM
  | ProtectedFM
  | PrivateFM
  | StaticFM
  | FinalFM
  | TransientFM
  | VolatileFM
  deriving (Show)

data MethodModifier
  = PublicMM
  | ProtectedMM
  | PrivateMM
  | AbstractMM
  | StaticMM
  | FinalMM
  | SynchronizedMM
  | NativeMM
  | StrictfpMM
  deriving (Show)

data MethodHeader
  = MethodHeader [TypeParameter] ResultType MethodDeclarator (Maybe Throws)
  deriving (Show)

data ResultType
  = ReturnRT Type
  | VoidRT
  deriving (Show)

data MethodDeclarator
  = MethodDeclarator Identifier (Maybe FormalParameterList) (Maybe Dims)
  deriving (Show)

data FormalParameterList
  = FormalParameterList (Maybe ReceiverParameter) [FormalParameter] (Maybe VarargParameter)
  deriving (Show)

data FormalParameter
  = FormalParameter [VariableModifier] Type VariableDeclaratorId
  deriving (Show)

data ReceiverParameter
  = ReceiverParameter Type (Maybe Identifier)
  deriving (Show)

data VarargParameter
  = VarargParameter [VariableModifier] Type VariableDeclaratorId
  deriving (Show)

data Throws
  = Throws [ExceptionType]
  deriving (Show)

data MethodBody
  = BlockMB Block
  | EmptyMB
  deriving (Show)

data ConstructorModifier
  = PublicConM
  | ProtectedConM
  | PrivateConM
  deriving (Show)

data ConstructorDeclarator
  = ConstructorDeclarator [TypeParameter] Identifier (Maybe FormalParameterList)
  deriving (Show)

data ConstructorBody
  = ConstructorBody (Maybe ExplicitConstructorInvocation) [Statement]
  deriving (Show)

data ExplicitConstructorInvocation
  = ThisECI [TypeArgument] [Expression]
  | SuperECI (Maybe Expression) [TypeArgument] [Expression]
  deriving (Show)

data TypeParameter
  = TypeParameter Identifier (Maybe TypeBound)
  deriving (Show)

data TypeBound
  = TypeBound (NonEmpty COIType)
  deriving (Show)

--------------------------------------------------
-- Statements

data Statement
  = BlockStatement Block
  | EmptyStatement
  | ExpressionStatement Expression
  | AssertStatement Expression (Maybe Expression)
  | SwitchStatement Expression [SwitchBlockStatementGroup] (Maybe SwitchLabel)
  | DoStatement Statement Expression
  | BreakStatement (Maybe Identifier)
  | ContinueStatement (Maybe Identifier)
  | ReturnStatement (Maybe Expression)
  | SynchronizedStatement Expression Block
  | ThrowStatement Expression
  | TryStatement Try
  | LabeledStatement Identifier Statement
  | IfThenStatement Expression Statement (Maybe Statement)
  | WhileStatement Expression Statement
  | ForStatement (Maybe ForInit) (Maybe Expression) [Expression] Statement
  | EnhancedForStatement [VariableModifier] Type VariableDeclaratorId Expression Statement
  deriving (Show)

data ForInit
  = ExpressionsFI [Expression]
  | LocalVariableDeclarationFI LocalVariableDeclaration
  deriving (Show)

data Try
  = Try Block (NonEmpty CatchClause)
  | FinallyTry Block [CatchClause] Block
  | ResourcesTry [Resource] Block [CatchClause] (Maybe Block)
  deriving (Show)

data CatchClause
  = CatchClause [VariableModifier] CatchType VariableDeclaratorId Block
  deriving (Show)

data CatchType
  = CatchType (NonEmpty ClassType)
  deriving (Show)

data Resource
  = Resource [VariableModifier] Type VariableDeclaratorId Expression
  deriving (Show)

data SwitchBlockStatementGroup
  = SwitchBlockStatementGroup (NonEmpty SwitchLabel) [Statement]
  deriving (Show)

data SwitchLabel
  = ConstantSL Expression
  | EnumSL Identifier
  | DefaultSL
  deriving (Show)
