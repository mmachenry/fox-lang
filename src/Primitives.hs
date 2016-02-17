{-# LANGUAGE LambdaCase #-}

module Primitives where

import Ast
import Exn
import State
import InterpM

primitives :: [(String, FoxValue)]
primitives = [
      ("unit", ValUnit)
    , ("true", ValBool True)
    , ("false", ValBool False)
    , numericOperator "+" (+)
    , numericOperator "-" (-) 
    , numericOperator "*" (*) 
    , numericOperator "/" (/)

    , booleanOperator "&&" (&&)
    , booleanOperator "||" (||)
    , ("not", typeWrap1 (pure . not) fromBool ValBool)

    -- FIXME: For now, implementing == and != as monomorphic operations
    -- on integers. This will need to change.
    , compOperator "==" (==)
    , compOperator "!=" (/=)
    , compOperator ">" (>)
    , compOperator "<" (<)
    , compOperator ">=" (>=)
    , compOperator "<=" (<=)

    -- State operations
    , ("newref", typeWrap1 allocateReference pure ValRef)
    , ("!", typeWrap1 getValue fromRef id)
    , (":=", ValPrimitive ":=" (\case
        [ValRef loc, val] -> do
            assignValue loc val
            return ValUnit
        _ -> throwError $ DynamicError "argument error."
        ))

    -- IO stuff
    , ("print", typeWrap1 (liftIO . print) pure (const ValUnit))
    , ("ignore", typeWrap1 (const $ pure ()) pure (const ValUnit))
    ]

allocateReference :: FoxValue -> EvalMonad ReferenceId
allocateReference val = do
    state <- get
    let (refid, newState) = addReference val state
    put newState
    return refid

getValue :: ReferenceId -> EvalMonad FoxValue
getValue refid = lookupReference refid <$> get

assignValue :: ReferenceId -> FoxValue -> EvalMonad ()
assignValue refid val = modify (updateReference refid val)

typeWrap1
    :: (a -> EvalMonad b)
    -> (FoxValue -> EvalMonad a)
    -> (b -> FoxValue)
    -> FoxValue
typeWrap1 func extract inject = ValPrimitive "unnamed" $ \case
    [arg1] -> do
        val1 <- extract arg1
        result <- func val1
        return $ inject result
    _ -> throwError $ DynamicError "Invalid number of arguments. Expected one."

liftPrimitive2
    :: String
    -> (a -> b -> c)
    -> (FoxValue -> EvalMonad a)
    -> (FoxValue -> EvalMonad b)
    -> (c -> FoxValue)
    -> FoxValue
liftPrimitive2 name func extract1 extract2 inject = ValPrimitive name $ \case
    [arg1, arg2] -> do
        val1 <- extract1 arg1
        val2 <- extract2 arg2
        return (inject (func val1 val2))
    _ -> throwError $ DynamicError "Invalid number of argments. Expected one."

fromBool :: FoxValue -> EvalMonad Bool
fromBool = \case
    ValBool b -> return b
    _ -> throwError $ DynamicError "Expected bool"

fromNum :: FoxValue -> EvalMonad FoxNum
fromNum = \case
    ValNum i -> return i
    _ -> throwError $ DynamicError "Expected num"

fromRef :: FoxValue -> EvalMonad ReferenceId
fromRef = \case
    ValRef refid -> return refid
    _ -> throwError $ DynamicError "Expected reference"

numericOperator :: String -> (FoxNum -> FoxNum -> FoxNum) -> (String, FoxValue)
numericOperator name f = (name, liftPrimitive2 name f fromNum fromNum ValNum)

booleanOperator :: String -> (Bool -> Bool -> Bool) -> (String, FoxValue)
booleanOperator name f = (name, liftPrimitive2 name f fromBool fromBool ValBool)

compOperator :: String -> (FoxNum -> FoxNum -> Bool) -> (String, FoxValue)
compOperator name f = (name, liftPrimitive2 name f fromNum fromNum ValBool)

