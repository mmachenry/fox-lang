module Eval (evalModule, evalExpr) where

import Ast
import Exn
import Primitives
import State
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import InterpM

evalModule :: Module -> IO (Either FoxExn FoxValue)
evalModule (Module definitions) = runEval $ local (const moduleEnvironment) $
    evalExpr $ ExprApp (ExprVar "main") []
    where moduleEnvironment = Map.fromList $ map makeClosure definitions
          makeClosure (Definition identifier params body) =
              (identifier, ValClosure moduleEnvironment params body)
 
evalExpr :: Expr -> EvalMonad FoxValue
evalExpr ast = case ast of
    ExprVar i -> do
        env <- ask
        case Map.lookup i env <|> Map.lookup i (Map.fromList primitives) of
            Just val -> return val
            Nothing -> throwError $ DynamicError $
                        "Reference to an unbound identifier: " ++ i

    ExprApp func args -> do
        funcVal <- evalExpr func
        argVals <- sequence $ fmap evalExpr args
        case funcVal of
            ValClosure closureEnv params body ->
                if length params == length args
                then let newEnv = Map.fromList $
                             zip (map parameterIdentifier params) argVals
                     in local (Map.union (Map.union closureEnv newEnv)) $
                            evalExpr body
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

    ExprRun expr -> modify pushHeap *> evalExpr expr <* modify popHeap

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
            v -> throwError $ DynamicError $
                     "Expected number for repeat" ++ show v

    ExprLiteral val -> return val

