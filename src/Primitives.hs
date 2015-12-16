{-# LANGUAGE LambdaCase #-}

module Primitives where

import Ast

primitives :: [(String, Value)]
primitives = [
      ("unit", ValUnit)
    , ("true", ValBool True)
    , ("false", ValBool False)
    , ("+", liftPrimitive2 "+" (+) fromNum fromNum ValNum)
    , numericOperator "-" (-) 
    , numericOperator "*" (*) 
    -- FIXME: For now, Fox only has integers and thus the divide operator
    -- is integer division
    , numericOperator "/" div

    , booleanOperator "&&" (&&)
    , booleanOperator "||" (||)
    , ("not", liftPrimitive1 not fromBool ValBool)

    -- FIXME: For now, implementing == and != as monomorphic operations
    -- on integers. This will need to change.
    , compOperator "==" (==)
    , compOperator "!=" (/=)
    , compOperator ">" (>)
    , compOperator "<" (<)
    , compOperator ">=" (>=)
    , compOperator "<=" (<=)

    -- For testing
    , ("test", ValPrimitive "test" (\args->case args of
        [ValNum i] -> Right $ ValNum (i+1)
        _ -> Left $ ErrorGeneric "argument error."
        ))
    , ("print", ValPrimitive "print" (\args->case args of
        [x] -> Right ValUnit
        _ -> Left $ ErrorGeneric "argument error."
        ))
    ]

liftPrimitive1 :: (a -> b) -> (Value -> Either Error a) -> (b -> Value) -> Value
liftPrimitive1 func extract inject = ValPrimitive "unnamed" $ \case
    [arg1] -> do
        val1 <- extract arg1
        Right (inject (func val1))
    _ -> Left $ ErrorGeneric "Invalid number of argments. Expected one."

liftPrimitive2
    :: String
    -> (a -> b -> c)
    -> (Value -> Either Error a)
    -> (Value -> Either Error b)
    -> (c -> Value)
    -> Value
liftPrimitive2 name func extract1 extract2 inject = ValPrimitive name $ \case
    [arg1, arg2] -> do
        val1 <- extract1 arg1
        val2 <- extract2 arg2
        Right (inject (func val1 val2))
    _ -> Left $ ErrorGeneric "Invalid number of argments. Expected one."

fromBool :: Value -> Either Error Bool
fromBool = \case
    ValBool b -> Right b
    _ -> Left $ ErrorGeneric "Expected bool"

fromNum :: Value -> Either Error Integer
fromNum = \case
    ValNum i -> Right i
    _ -> Left $ ErrorGeneric "Expected num"

numericOperator :: String -> (Integer -> Integer -> Integer) -> (String, Value)
numericOperator name f = (name, liftPrimitive2 name f fromNum fromNum ValNum)

booleanOperator :: String -> (Bool -> Bool -> Bool) -> (String, Value)
booleanOperator name f = (name, liftPrimitive2 name f fromBool fromBool ValBool)

compOperator :: String -> (Integer -> Integer -> Bool) -> (String, Value)
compOperator name f = (name, liftPrimitive2 name f fromNum fromNum ValBool)

