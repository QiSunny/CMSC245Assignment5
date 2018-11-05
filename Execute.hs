-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Java.Execute
-- Copyright   :  (C) 2018 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  portable
--
-- Execute Java statements and evaluate Java expressions
--
----------------------------------------------------------------------------

module Language.Java.Execute ( Result(..), execStatement, evalExpression ) where

import Language.Java.Char
import Language.Java.Lang
import Language.Java.Literal
import Language.Java.Monad
import Language.Java.Mutate
import Language.Java.Panic
import Language.Java.State
import Language.Java.String
import Language.Java.Syntax
import Language.Java.Type
import Language.Java.TypeCheck
import Language.Java.Value

import Data.Fixed  ( mod' )
import Data.Int

-- | Result of running an expression is a variable, value, or nothing (S15.1)
data Result
  = ValueR Value
-- TODO  | VariableR
  | NothingR
  deriving (Show)

-- | Execute a Java statement
execStatement :: Statement -> Java Result
execStatement (ExpressionStatement expr) = evalExpression expr
execStatement other = issueError $ "I don't know how to execute " ++ show other

-- | Evaluate a Java expression
evalExpression :: Expression -> Java Result
evalExpression (BinaryE lhs op rhs) = evalBinary op lhs rhs
evalExpression (LiteralE lit) = do
  value <- evalLit lit
  pure (ValueR value)
evalExpression other = issueError $ "I don't know how to evaluate " ++ show other

-- | Evaluate a binary expression
evalBinary :: BinaryOperator -> Expression -> Expression -> Java Result
evalBinary op lhs_expr rhs_expr = do
  -- S15.7.1 says to evaluate the left side first (at least, in the implemented cases)
  lhs_res <- evalExpression lhs_expr
  rhs_res <- evalExpression rhs_expr

  -- all the binary operators use values
  lhs_val <- getValue lhs_res
  rhs_val <- getValue rhs_res

  val <- performBinaryOp op lhs_val rhs_val
  pure (ValueR val)

{-
Note [Detecting string concatenation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
According to S15.18, the decision to be string concatenation or
not is based on the *compile-time* types of the operands, not the
runtime types. This matters only if the String involved is null.
Because null Strings are unusual, I've decided that a null value
should be considered not to be a String.

What this means in practice is that the following will throw a
NullPointerException:

  String s = null;
  String s2 = s + 4;

In standards-compliant Java, this should assign s2 to the string
"null4".

Why not just use the type? Because that makes + to be type-directed.
I want type-checking to be optional, so that we can experiment with
what happens when we turn type-checking off.
-}

-- | Actually compute the result of the operation (for operators with
-- normal evaluation order)
performBinaryOp :: BinaryOperator -> Value -> Value -> Java Value
performBinaryOp Plus lhs_val rhs_val = do
  -- now, need to see if this is string concatenation or not
  lhs_is_str <- isStringValue lhs_val
  rhs_is_str <- isStringValue rhs_val
    -- See Note [Detecting string concatenation]

  if lhs_is_str || rhs_is_str
    -- S15.18: "If the type of either operand of a + operator is String,
    --          then the operation is string concatenation."
    then do -- S15.18.1
      lhs_str <- stringConversion lhs_val  -- it's safe to use string conversion
      rhs_str <- stringConversion rhs_val  -- on the string, too, so convert both
       -- NB: We ignore the bit about constant expressions; see README
      let concatenated = appendStrings lhs_str rhs_str
      result_ref <- allocString concatenated
      pure (ReferenceV result_ref)
    else -- S15.18.2
      performNumericalOp (+) (+) (+) (+) noCheck lhs_val rhs_val

-- S15.18, S15.17
performBinaryOp Minus     lhs rhs = performNumericalOp (-) (-) (-) (-) noCheck lhs rhs
performBinaryOp Times     lhs rhs = performNumericalOp (*) (*) (*) (*) noCheck lhs rhs
performBinaryOp DividedBy lhs rhs = performNumericalOp div div (/) (/) checkDivisor lhs rhs
performBinaryOp Modulus   lhs rhs = performNumericalOp mod mod mod' mod' checkDivisor lhs rhs
performBinaryOp other _ _ = unimplementedM $ "perform " ++ show other

-- | This function performs an operation on numbers. First, binary numeric promotion
-- is performed, so that we have something in the set {int, long, float, double}.
-- Then, a check is performed on the values after promotion, to see if we need to
-- throw any exceptions. (This allows for integer division to check for 0.) Lastly,
-- the appropriate operation is performed and the result returned.
--
-- performNumericalOp takes four operation functions as arguments, one each for the
-- different possibilities of the type of the Java values. A caller must thus provide
-- an operation on ints, on longs, on floats, and on doubles.
--
-- The caller must also provide a checking function. See 'noCheck' and 'checkDivisor'
-- for the checking function you probably want.
--
-- Note that the operation functions might return any type, as long as values of that
-- type can be converted into a Java value. This is controlled by the ValueType
-- constraint.
performNumericalOp :: ( ValueType v_int, ValueType v_long
                      , ValueType v_float, ValueType v_double )
                   => (Int32 -> Int32 -> v_int)
                   -> (Int64 -> Int64 -> v_long)
                   -> (Float -> Float -> v_float)
                   -> (Double -> Double -> v_double)
                   -> (Value -> Value -> Java ())
                   -> Value -> Value
                   -> Java Value
performNumericalOp op_int op_long op_float op_double check lhs_val rhs_val = do
  (lhs_prom, rhs_prom) <- binaryNumericPromotion lhs_val rhs_val
  check lhs_prom rhs_prom
  pure $ case (lhs_prom, rhs_prom) of
    (IntV l,    IntV r)    -> toValue $ op_int l r
    (LongV l,   LongV r)   -> toValue $ op_long l r
    (FloatV l,  FloatV r)  -> toValue $ op_float l r
    (DoubleV l, DoubleV r) -> toValue $ op_double l r
    _                      -> panic $ "Wrong types after binary numeric promotion: \n" ++
                                      show lhs_prom ++ "\n" ++
                                      show rhs_prom

-- | This checking function (for use with 'performNumericalOp') does no checks,
-- always succeeding.
noCheck :: Value -> Value -> Java ()
noCheck _ _ = pure ()

-- | This checking function (for use with 'performNumericalOp') checks that the
-- second parameter is not an integral 0.
checkDivisor :: Value -> Value -> Java ()
checkDivisor _ (IntV 0)  = throwJavaException "ArithmeticException: / by zero"
checkDivisor _ (LongV 0) = throwJavaException "ArithmeticException: / by zero"
checkDivisor _ _         = pure ()

-- | Evaluate a literal to a value
-- In the case of Strings, this actually is stateful.
evalLit :: Literal -> Java Value
evalLit (IntL n)      = pure (IntV n)
evalLit (LongL n)     = pure (LongV n)
evalLit (FloatL n)    = pure (FloatV n)
evalLit (DoubleL n)   = pure (DoubleV n)
evalLit (BooleanL b)  = pure (BooleanV b)
evalLit (CharL c)     = pure (CharV c)
evalLit NullL         = pure (ReferenceV Null)
evalLit (StringL str) = do str_ref <- allocString str
                           pure (ReferenceV str_ref)

-------------------------------------------------------------
-- Conversions (from Chapter 5)

-- | Perform widening primitive conversion, as specified in S5.1.2.
-- Also allows identity conversions.
wideningPrimitiveConversion :: Value           -- source value
                            -> PrimitiveType   -- target type
                            -> Value           -- widened value
wideningPrimitiveConversion val ty
  | primitiveValueType val == ty = val

wideningPrimitiveConversion (ByteV n) Short  = ShortV (fromIntegral n)
wideningPrimitiveConversion (ByteV n) Int    = IntV (fromIntegral n)
wideningPrimitiveConversion (ByteV n) Long   = LongV (fromIntegral n)
wideningPrimitiveConversion (ByteV n) Float  = FloatV (fromIntegral n)
wideningPrimitiveConversion (ByteV n) Double = DoubleV (fromIntegral n)

wideningPrimitiveConversion (ShortV n) Int    = IntV (fromIntegral n)
wideningPrimitiveConversion (ShortV n) Long   = LongV (fromIntegral n)
wideningPrimitiveConversion (ShortV n) Float  = FloatV (fromIntegral n)
wideningPrimitiveConversion (ShortV n) Double = DoubleV (fromIntegral n)

wideningPrimitiveConversion (CharV n) Int    = IntV (fromIntegral n)
wideningPrimitiveConversion (CharV n) Long   = LongV (fromIntegral n)
wideningPrimitiveConversion (CharV n) Float  = FloatV (fromIntegral n)
wideningPrimitiveConversion (CharV n) Double = DoubleV (fromIntegral n)

wideningPrimitiveConversion (IntV n) Long   = LongV (fromIntegral n)
wideningPrimitiveConversion (IntV n) Float  = FloatV (fromIntegral n)
wideningPrimitiveConversion (IntV n) Double = DoubleV (fromIntegral n)

wideningPrimitiveConversion (LongV n) Float  = FloatV (fromIntegral n)
wideningPrimitiveConversion (LongV n) Double = DoubleV (fromIntegral n)

wideningPrimitiveConversion (FloatV n) Double = DoubleV (realToFrac n)

wideningPrimitiveConversion val ty
  = panic $ "Cannot do widening primitive conversion from " ++ show val ++ " to " ++ show ty

-- | Performs unboxing conversion, as specified in S5.1.8, but permits
-- the input to be a non-reference value, returning it untouched.
-- If the input is null, this throws a NullPointerException, as specified.
-- Postcondition: The output is not a reference.
unboxConversion :: Value -> Java Value
unboxConversion (ReferenceV ref) = do
  heap_elt <- dereference ref
  pure $ case heap_elt of
    ObjectHE obj -> unboxObject obj
    ArrayHE arr  -> panic $ "Unboxing conversion found an array: " ++ show arr
unboxConversion other = pure other

-- | Perform string conversion, as specified in S5.1.11
stringConversion :: Value -> Java JString
stringConversion (ReferenceV Null)          = pure (stringHtoJ "null")
stringConversion (ReferenceV (Pointer ptr)) = do
    heap_elt <- dereferenceRaw ptr
    -- According to S5.1.11, we should call toString now. But we don't have
    -- method calls, etc., working yet. So we'll just see if the reference
    -- is a String and fail otherwise
    case heap_elt of
      ObjectHE obj | isStringObject obj -> getString obj
      _                                 -> unimplementedM "call toString()"

  -- For primitive values, the JLS says to box and then call toString. It also says
  -- that an implementation is free to skip that step if it likes. It likes.
stringConversion (ByteV n)        = pure (jshow n)
stringConversion (ShortV n)       = pure (jshow n)
stringConversion (IntV n)         = pure (jshow n)
stringConversion (LongV n)        = pure (jshow n)
stringConversion (CharV c)        = pure (JString [c])
stringConversion (FloatV n)       = pure (jshow n)
stringConversion (DoubleV n)      = pure (jshow n)
stringConversion (BooleanV True)  = pure (stringHtoJ "true")
stringConversion (BooleanV False) = pure (stringHtoJ "false")

-- | Promote a pair of values to be the same numeric type, according to
-- the Binary Numeric Promotion algorithm of S5.6.2.
-- Postcondition: both values have the same type, one of int, long, float, or double
binaryNumericPromotion :: Value -> Value -> Java (Value, Value)
binaryNumericPromotion lhs_val rhs_val = do
  lhs_primval <- unboxConversion lhs_val
  rhs_primval <- unboxConversion rhs_val

  let lhs_ty = primitiveValueType lhs_primval
      rhs_ty = primitiveValueType rhs_primval

  pure $ case (lhs_ty, rhs_ty) of
    (Double, _)      -> (lhs_primval, wideningPrimitiveConversion rhs_primval Double)
    (_, Double)      -> (wideningPrimitiveConversion lhs_primval Double, rhs_primval)
    (Float, _)       -> (lhs_primval, wideningPrimitiveConversion rhs_primval Float)
    (_, Float)       -> (wideningPrimitiveConversion lhs_primval Float, rhs_primval)
    (Long, _)        -> (lhs_primval, wideningPrimitiveConversion rhs_primval Long)
    (_, Long)        -> (wideningPrimitiveConversion lhs_primval Long, rhs_primval)
    _                -> ( wideningPrimitiveConversion lhs_primval Int
                        , wideningPrimitiveConversion rhs_primval Int )
    -- NB: We don't do value set conversion, contradicting the spec.

-------------------------------------------------------------
-- Utility

-- | Convert a 'Result' to a 'Value', issuing an error for a 'NothingR' result.
getValue :: Result -> Java Value
getValue (ValueR val) = pure val
getValue NothingR     = issueError "Unexpected void result"

-- | Check whether a Value denotes a String. null values are considered not
-- strings.
isStringValue :: Value -> Java Bool
isStringValue (ReferenceV (Pointer ptr)) = do
  heap_elt <- dereferenceRaw ptr
  pure (isStringHeapElement heap_elt)
isStringValue _other = pure False
