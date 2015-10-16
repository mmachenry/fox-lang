module Ast (
    Module(..),
    Definition(..),
    Pattern(..),
    Expr(..),
    Operator(..)
    ) where

type Identifier = String

data Parameter = Parameter Identifier Type
    deriving (Show, Eq)

data Type = InferredType
    deriving (Show, Eq)

data Module = Module [Definition] deriving (Eq, Show)

data Definition = Definition Identifier [Pattern] Expr deriving (Eq, Show)

data Pattern =
      PatternId Identifier
    | PatternTuple [Pattern]
    deriving (Eq, Show)

data Expr =
    -- Core syntax
      ExprVar Identifier
    | ExprApp Expr Expr
    | ExprAbs [Parameter] Expr
    | ExprLetBind Identifier Expr Expr
    | ExprEffectBind Identifier Expr Expr
    | ExprRun Expr

    -- Other assumed syntax
    | ExprIfThenElse Expr Expr Expr
    | ExprMatch Expr [(Pattern, Expr)]
    | ExprBinop Operator Expr Expr
    | ExprCompound Expr Expr

    -- References
    | ExprAlloc Expr
    | ExprRead Expr
    | ExprWrite Expr Expr

    -- Literal
    | ExprNum Integer
    | ExprTuple [Expr]
    | ExprNil -- likely this is just defined in language as an AST

    deriving (Show, Eq)

data Operator =
      Add | Sub | Mul | Div
    | BoolEq | Ne | Gt | Lt | Gte | Lte
    | And | Or
    -- | Not, Negate
    deriving (Show, Eq)

