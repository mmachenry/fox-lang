module Eval (eval) where

import Ast

data Error = Error String

data Value =
      ValInt Int
    | ValBool Bool

evalModule :: Module -> Either Error Value
evalModule (Module definitions) = undefined

evalExpr :: Expr -> Either Error Value
evalExpr ast = case ast of
    ExprVar id -> undefined
    ExprApp func args -> undefined
    ExprAbs params body -> undefined
    ExprLetBind id expr -> undefined
    ExprEffectBind id expr -> undefined
    ExprRun exprs -> undefined

    ExprIfThenElse test consequent alternate -> undefined
    ExprMatch expr cases -> undefined
    ExprRepeat numTimes exprs -> undefined
    ExprStatementBlock exprs -> undefined
    ExprBinOp binOp lhs rhs -> undefined
    ExprUnaryOp unaryOp operand -> undefined

    ExprAlloc expr -> undefined
    ExprRead expr -> undefined
    ExprWrite lhs rhs -> undefined

    ExprNum integer -> ValInt integer
