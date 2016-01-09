module Eval (evalModule, evalExpr) where

import Ast
import Primitives
import State
import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader
import Control.Monad
import qualified Data.Map as Map

evalModule :: Module -> IO (Either FoxError Value)
evalModule (Module definitions) = runEval $
    local (const (Map.union moduleEnvironment (Map.fromList primitives))) $
        evalExpr $ ExprApp (ExprVar "main") []
    where moduleEnvironment = Map.fromList $ map makeClosure definitions
          makeClosure (Definition identifier params body) =
              (identifier, ValClosure moduleEnvironment params body)
 
evalExpr :: Expr -> EvalMonad Value
evalExpr ast = case ast of
    ExprVar i -> do
        env <- ask
        case Map.lookup i env of
            Just val -> return val
            Nothing -> throwError $ DynamicError $
                        "Reference to an unbound identifier: " ++ i

    ExprApp func args -> do
        funcVal <- evalExpr func
        argVals <- sequence $ fmap evalExpr args
        case funcVal of
            ValClosure closureEnv params body ->
                if length params == length args
                then let newEnv = Map.fromList $ zip (map parameterIdentifier params) argVals
                     in local (Map.union newEnv) $ evalExpr body
                else throwError $ DynamicError "Mismatch number of parameters."
            ValPrimitive _ prim -> prim argVals
            _ -> throwError $ DynamicError "Applying a non-function."

    ExprAbs params body -> do
        env <- ask
        return $ ValClosure env params body

    ExprLetBind id expr body -> do
        val <- evalExpr expr
        local (Map.insert id val) $ evalExpr body

    ExprEffectBind id expr body -> do
        val <- evalExpr expr
        local (Map.insert id val) $ evalExpr body

    ExprCompound expr1 expr2 -> evalExpr expr1 >> evalExpr expr2

    ExprRun expr -> do
        modify pushHeap
        value <- evalExpr expr
        modify popHeap
        return value

    ExprIfThenElse test consequent alternate -> do
        testValue <- evalExpr test
        case testValue of
            ValBool True -> evalExpr consequent
            ValBool False -> evalExpr alternate
            _ -> throwError $ DynamicError "Condition of IF expected a boolean."

    ExprMatch expr cases -> undefined

    ExprRepeat numTimes expr -> do
        numval <- evalExpr numTimes
        case numval of
            ValNum n -> do replicateM_ (ceiling n) (evalExpr expr)
                           return ValUnit
            v -> throwError $ DynamicError $ "Expected number for repeat" ++ show v

    ExprNum integer -> return $ ValNum integer

