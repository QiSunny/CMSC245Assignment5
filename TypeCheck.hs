{-# LANGUAGE OverloadedLists #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.TypeCheck
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- A Java type checker
--
----------------------------------------------------------------------------

module Language.Java.TypeCheck (
  typeCheckStatement, typeCheckExpression,

  primitiveValueType

  ) where

import Language.Java.Syntax
import Language.Java.Monad
import Language.Java.Lang
import Language.Java.Literal
import Language.Java.Panic
import Language.Java.Pretty
import Language.Java.Type
import Language.Java.Value

import Control.Monad

typeCheckStatement :: Statement -> Java ()
typeCheckStatement (ExpressionStatement expr) = void (typeCheckExpression expr)
typeCheckStatement stmt                       = unimplementedM $ "type-check this statement: " ++ show stmt

typeCheckExpression :: Expression -> Java Type
typeCheckExpression (BinaryE lhs op rhs) = do
  lhs_ty <- typeCheckExpression lhs
  rhs_ty <- typeCheckExpression rhs
  binaryExpressionType op lhs_ty rhs_ty
typeCheckExpression (LiteralE lit) = pure (litType lit)
typeCheckExpression other = unimplementedM $ "type-check this expression:" ++ show other

-- | Check (and return) the type of a binary operator expression
binaryExpressionType :: BinaryOperator
                     -> Type -> Type   -- left-hand and right-hand types
                     -> Java Type
binaryExpressionType Plus lhs_ty rhs_ty -- S15.18
  | isStringType lhs_ty || isStringType rhs_ty  -- S15.18.1
  = pure stringType
  | otherwise -- S15.18.2
  = arithmeticExpressionType lhs_ty rhs_ty
binaryExpressionType op lhs_ty rhs_ty
  |  op == Minus     -- S15.18.2
  || op == Times     -- S15.17
  || op == DividedBy
  || op == Modulus
  = arithmeticExpressionType lhs_ty rhs_ty
  | op == LessThan || op == GreaterThan || op == LessThanEquals || op == GreaterThanEquals
  = shiftExpressionType lhs_ty rhs_ty

binaryExpressionType other _ _ = unimplementedM $ "type-check binary expressions with " ++ show other

shiftExpressionType :: Type -> Type -> Java Type
shiftExpressionType lhs_ty rhs_ty = do
  checkRelationalExpression lhs_ty
  checkRelationalExpression rhs_ty

  ref_type <- binaryNumericReferenceType lhs_ty rhs_ty
  pure (RefType ref_type)

-- | Computes the type of an arithmetic expression, which is one headed
-- by + (not on strings), -, *, /, or %
arithmeticExpressionType :: Type -> Type -> Java Type
arithmeticExpressionType lhs_ty rhs_ty = do
  checkConvertibleToNumericType lhs_ty
  checkConvertibleToNumericType rhs_ty

  prim_type <- binaryNumericPromotionType lhs_ty rhs_ty
  pure (PrimType prim_type)

-- | Get the type of a literal
litType :: Literal -> Type
litType (IntL _)       = PrimType Int
litType (LongL _)      = PrimType Long
litType (FloatL _)     = PrimType Float
litType (DoubleL _)    = PrimType Double
litType (BooleanL _)   = PrimType Boolean
litType (CharL _)      = PrimType Char
litType (StringL _)    = stringType
litType NullL          = PrimType NullType

-----------------------------------------------------------------------
-- Conversions

-- | Checks that the given widening primitive conversion (S5.1.2) is possible.
-- If so, returns the converted type (always the second argument). Allows the
-- two types to be equal. If the conversion is not possible, issues an error.
wideningPrimitiveConversionType :: PrimitiveType   -- "from" type
                                -> PrimitiveType   -- "to" type
                                -> Java PrimitiveType   -- resulting "to" type
wideningPrimitiveConversionType from to = case (from, to) of
  _ | from == to  -> ok

  (Byte, Short)   -> ok
  (Byte, Int)     -> ok
  (Byte, Long)    -> ok
  (Byte, Float)   -> ok
  (Byte, Double)  -> ok

  (Short, Int)    -> ok
  (Short, Long)   -> ok
  (Short, Float)  -> ok
  (Short, Double) -> ok

  (Char, Int)     -> ok
  (Char, Long)    -> ok
  (Char, Float)   -> ok
  (Char, Double)  -> ok

  (Int, Long)     -> ok
  (Int, Float)    -> ok
  (Int, Double)   -> ok

  (Long, Float)   -> ok
  (Long, Double)  -> ok

  (Float, Double) -> ok

  _ -> issueError $ "Cannot perform a widening conversion from " ++ ppr from ++ " to " ++ ppr to

  where
    ok = pure to

-- | Tries to unbox a type, according to S5.1.8. If the type is already unboxed,
-- just returns the unboxed (primitive) type. If unboxing succeeds, returns Just
-- the primitive type. If this fails, returns Nothing.
unboxConversionType_maybe :: Type -> Maybe PrimitiveType
unboxConversionType_maybe (PrimType NullType) = Nothing  -- last bullet in S5.1.8
unboxConversionType_maybe (PrimType prim)     = Just prim
unboxConversionType_maybe (RefType ref_type)  = unboxType ref_type

-- | Computes the type you would get after performing unboxing conversion on
-- a value of the type given. If the type is already primitive, just returns
-- that type. Otherwise, if unboxing conversion is not possible, issues an
-- error.
unboxConversionType :: Type -> Java PrimitiveType
unboxConversionType (PrimType NullType) = issueError "Cannot unbox a literal `null`."
unboxConversionType (PrimType prim) = pure prim
unboxConversionType (RefType ref_type) = case unboxType ref_type of
  Just prim_type -> pure prim_type
  Nothing        -> issueError $ "Cannot perform unboxing on something of type " ++ ppr ref_type

-- | Checks if the argument is convertible to a numeric type (end of S5.1.8).
-- Returns True if the argument type is convertible.
isConvertibleToNumericType :: Type -> Bool
isConvertibleToNumericType ty
  | Just prim_ty <- unboxConversionType_maybe ty
  = case prim_ty of
      Byte   -> True
      Short  -> True
      Int    -> True
      Long   -> True
      Char   -> True
      Float  -> True
      Double -> True
      _      -> False
  | otherwise
  = False

-- | Checks if the argument is convertible to a numeric type (end of S5.1.8).
-- Issues an error if it isn't.
checkConvertibleToNumericType :: Type -> Java ()
checkConvertibleToNumericType ty
  | isConvertibleToNumericType ty
  = pure ()
  | otherwise
  = issueError (ppr ty ++ " is not convertible to a numeric type.")

-- | Computes the type produced by binary numeric promotion (S5.6.2),
-- including unboxing conversion (S5.1.8). Errors if either type is not convertible
-- to a numeric type (S5.1.8) or if binary numeric promotion fails.
binaryNumericPromotionType :: Type -> Type -> Java PrimitiveType
binaryNumericPromotionType lhs_ty rhs_ty = do
  lhs_unboxed <- unboxConversionType lhs_ty
  rhs_unboxed <- unboxConversionType rhs_ty

  case (lhs_unboxed, rhs_unboxed) of
    (Double, other) -> wideningPrimitiveConversionType other Double
    (other, Double) -> wideningPrimitiveConversionType other Double
    (Float, other) -> wideningPrimitiveConversionType other Float
    (other, Float) -> wideningPrimitiveConversionType other Float
    (Long, other) -> wideningPrimitiveConversionType other Long
    (other, Long) -> wideningPrimitiveConversionType other Long
    _ -> do _ <- wideningPrimitiveConversionType lhs_unboxed Int
            wideningPrimitiveConversionType rhs_unboxed Int

binaryNumericReferenceType :: Type -> Type -> Java ReferenceType
binaryNumericReferenceType lhs_ty rhs_ty = do
  lhs_unboxed <- unboxConversionType lhs_ty
  rhs_unboxed <- unboxConversionType rhs_ty

--  case (lhs_unboxed, rhs_unboxed) of
--    ()

-----------------------------------------------------------------------
-- Utility

-- | Get the type of a primitive value. Panics on reference values.
primitiveValueType :: Value -> PrimitiveType
primitiveValueType (ByteV _)    = Byte
primitiveValueType (ShortV _)   = Short
primitiveValueType (IntV _)     = Int
primitiveValueType (LongV _)    = Long
primitiveValueType (CharV _)    = Char
primitiveValueType (FloatV _)   = Float
primitiveValueType (DoubleV _)  = Double
primitiveValueType (BooleanV _) = Boolean
primitiveValueType other        = panic $ "primitiveValueType: not a primitive value: " ++ show other

referenceValueType :: Value -> ReferenceType
-- primitiveValueType (ReferenceV _) = 
