module Primitives where

import Ast

primitives :: [(String, Value)]
primitives = [
      ("unit", ValUnit)
    , ("true", ValBool True)
    , ("false", ValBool False)
    , numericOperator "+" (+)
    , numericOperator "-" (-) 
    , numericOperator "*" (*) 
    -- FIXME: For now, Fox only has integers and thus the divide operator
    -- is integer division
    , numericOperator "/" div

    , booleanOperator "&&" (&&)
    , booleanOperator "||" (||)

    -- FIXME: For now, implementing == and != as monomorphic operations
    -- on integers. This will need to change.
    , comparisonOperator "==" (==)
    , comparisonOperator "!=" (/=)
    , comparisonOperator ">" (>)
    , comparisonOperator "<" (<)
    , comparisonOperator ">=" (>=)
    , comparisonOperator "<=" (<=)

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

numericOperator :: String -> (Integer -> Integer -> Integer) -> (String, Value)
numericOperator name f =
    (name, ValPrimitive name (\args->case args of
        [ValNum lhs, ValNum rhs] -> Right $ ValNum (f lhs rhs)
        _ -> Left $ ErrorGeneric "Incorrect aguments."))

booleanOperator :: String -> (Bool -> Bool -> Bool) -> (String, Value)
booleanOperator name f =
    (name, ValPrimitive name (\args->case args of
        [ValBool lhs, ValBool rhs] -> Right $ ValBool (f lhs rhs)
        _ -> Left $ ErrorGeneric "Incorrect arguments."))

comparisonOperator :: String -> (Integer -> Integer -> Bool) -> (String, Value)
comparisonOperator name f =
    (name, ValPrimitive name (\args->case args of
        [ValNum lhs, ValNum rhs] -> Right $ ValBool (f lhs rhs)
        _ -> Left $ ErrorGeneric "Incorrect arguments."))
