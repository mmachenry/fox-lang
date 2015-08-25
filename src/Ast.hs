module Ast (
    Expr(..),
    Operator(..)
    ) where

type Identifier = String

data Parameter = Parameter Identifier Type
    deriving (Show, Eq)

data Type = InferredType
    deriving (Show, Eq)

data Expr =
    -- Core syntax
      ExprVar Identifier
    | ExprApp Expr [Expr]
    | ExprAbs [Parameter] Expr
    | ExprLetBind Identifier Expr Expr
    | ExprEffectBind Identifier Expr Expr
    | ExprRun Expr

    -- Other assumed syntax
    | ExprIfThenElse Expr Expr Expr
    | ExprBinop Operator Expr Expr
    | ExprCompound Expr Expr

    -- References
    | ExprAlloc Expr
    | ExprRead Expr
    | ExprWrite Expr Expr

    -- Literal
    | ExprNum Integer
    | ExprNil

    deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div deriving (Show, Eq)
