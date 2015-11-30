module Eval (evalModule, evalExpr, Value(..), Error(..)) where

import Ast

type Env = [(Identifier, Value)]

data Error =
      ErrorGeneric String
    deriving (Eq, Show)

data Value =
      ValNum Integer
    | ValBool Bool
    | ValClosure Env [Parameter] Expr
    deriving (Eq, Show)

evalModule :: Module -> Either Error Value
evalModule (Module definitions) = undefined

evalExpr :: Env -> Expr -> Either Error Value
evalExpr env ast = case ast of
    ExprVar id -> case lookup id env of
        Just val -> Right val
        Nothing -> Left $ ErrorGeneric "Reference to an unbound identifier."

    ExprApp func args -> do
        evaledFunc <- evalExpr env func
        case evaledFunc of
            ValClosure closureEnv params body ->
                if length params == length args
                then do evaledArgs <- sequence $ fmap (evalExpr env) args
                        let newEnv = zip (map parameterIdentifier params) evaledArgs
                        evalExpr newEnv body
                else Left $ ErrorGeneric "Mismatch number of parameters."
            _ -> Left $ ErrorGeneric "Applying a non-function."

    ExprAbs params body -> Right $ ValClosure env params body
    ExprLetBind id expr -> undefined
    ExprEffectBind id expr -> undefined
    ExprRun exprs -> undefined

    ExprIfThenElse test consequent alternate -> do
        testValue <- evalExpr env test
        case testValue of
            ValBool True -> evalExpr env consequent
            ValBool False -> evalExpr env alternate
            _ -> Left $ ErrorGeneric "Condition of IF expression expected a boolean."

    ExprMatch expr cases -> undefined
    ExprRepeat numTimes exprs -> undefined
    ExprStatementBlock exprs -> undefined
    ExprBinOp binOp lhs rhs -> undefined
    ExprUnaryOp unaryOp operand -> undefined

    ExprAlloc expr -> undefined
    ExprRead expr -> undefined
    ExprWrite lhs rhs -> undefined

    ExprNum integer -> Right $ ValNum integer
    ExprBool bool -> Right $ ValBool bool

