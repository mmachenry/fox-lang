module Eval (evalModule, evalExpr) where

import Ast
import Primitives
import Control.Applicative

evalModule :: Module -> Either Error Value
evalModule (Module definitions) =
    evalExpr moduleEnvironment $ ExprApp (ExprVar "main") []
    where moduleEnvironment = map makeClosure definitions
          makeClosure (Definition identifier params body) =
              (identifier, ValClosure moduleEnvironment params body)
 
evalExpr :: Env -> Expr -> Either Error Value
evalExpr env ast = case ast of
    ExprVar i -> case lookup i env <|> lookup i primitives of
        Just val -> Right val
        Nothing -> Left $ DynamicError $
                       "Reference to an unbound identifier: " ++ i

    ExprApp func args -> do
        funcVal <- evalExpr env func
        argVals <- sequence $ fmap (evalExpr env) args
        case funcVal of
            ValClosure closureEnv params body ->
                if length params == length args
                then let newEnv = zip (map parameterIdentifier params) argVals
                     in evalExpr (newEnv++closureEnv) body
                else Left $ DynamicError "Mismatch number of parameters."
            ValPrimitive _ prim -> prim argVals
            _ -> Left $ DynamicError "Applying a non-function."

    ExprAbs params body -> Right $ ValClosure env params body

    ExprLetBind id expr body -> do
        val <- evalExpr env expr
        evalExpr ((id,val):env) body

    ExprEffectBind id expr body -> do
        val <- evalExpr env expr
        evalExpr ((id,val):env) body

    ExprCompound expr1 expr2 -> evalExpr env expr1 >> evalExpr env expr2

    ExprRun expr -> undefined

    ExprIfThenElse test consequent alternate -> do
        testValue <- evalExpr env test
        case testValue of
            ValBool True -> evalExpr env consequent
            ValBool False -> evalExpr env alternate
            _ -> Left $ DynamicError "Condition of IF expected a boolean."

    ExprMatch expr cases -> undefined

    ExprRepeat numTimes expr -> undefined

    ExprNum integer -> Right $ ValNum integer

