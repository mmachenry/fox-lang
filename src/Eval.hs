module Eval (evalModule, evalExpr, Value(..), Error(..)) where

import Ast
import Primitives
import Control.Applicative

evalModule :: Module -> Either Error Value
evalModule (Module definitions) = undefined

evalExpr :: Env -> Expr -> Either Error Value
evalExpr env ast = case ast of
    ExprVar id -> case lookup id env <|> lookup id primitives of
        Just val -> Right val
        Nothing -> Left $ ErrorGeneric $
                       "Reference to an unbound identifier: " ++ id

    ExprApp func args -> do
        evaledFunc <- evalExpr env func
        case evaledFunc of
            ValClosure closureEnv params body ->
                if length params == length args
                then do evaledArgs <- sequence $ fmap (evalExpr env) args
                        let newEnv = zip (map parameterIdentifier params)
                                         evaledArgs
                        evalExpr newEnv body
                else Left $ ErrorGeneric "Mismatch number of parameters."
            ValPrimitive _ prim -> do
                evaledArgs <- sequence $ fmap (evalExpr env) args
                prim evaledArgs
            _ -> Left $ ErrorGeneric "Applying a non-function."

    ExprAbs params body -> Right $ ValClosure env params body

    ExprLetBind id expr body -> do
        val <- evalExpr env expr
        evalExpr ((id,val):env) body

    ExprEffectBind id expr body -> undefined

    ExprCompound expr1 expr2 ->
        evalExpr env expr1 >> evalExpr env expr2

    ExprRun exprs -> undefined

    ExprIfThenElse test consequent alternate -> do
        testValue <- evalExpr env test
        case testValue of
            ValBool True -> evalExpr env consequent
            ValBool False -> evalExpr env alternate
            _ -> Left $ ErrorGeneric "Condition of IF expression expected a boolean."

    ExprMatch expr cases -> undefined
    ExprRepeat numTimes exprs -> undefined
    ExprBinOp binOp lhs rhs -> undefined
    ExprUnaryOp unaryOp operand -> undefined

    ExprAlloc expr -> undefined
    ExprRead expr -> undefined
    ExprWrite lhs rhs -> undefined

    ExprNum integer -> Right $ ValNum integer
    ExprBool bool -> Right $ ValBool bool

