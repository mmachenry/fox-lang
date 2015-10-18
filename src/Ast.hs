module Ast (
    Module(..),
    Definition(..),
    Parameter(..),
    Pattern(..),
    Expr(..),
    Operator(..),
    Type(..)
    ) where

type Identifier = String

data Type = TypeInfered | TypeInt deriving (Show, Eq)

data Module = Module [Definition] deriving (Eq, Show)

data Definition = Definition Identifier [Parameter] [Expr] deriving (Eq, Show)

data Parameter = Parameter Identifier Type deriving (Eq, Show)

data Pattern =
      PatternId Identifier
    | PatternTuple [Pattern]
    deriving (Eq, Show)

data Expr =
    -- Core syntax
      ExprVar Identifier
    | ExprApp Expr Expr
    | ExprAbs [Parameter] Expr
    | ExprLetBind Identifier Expr
    | ExprEffectBind Identifier Expr
    | ExprRun [Expr]

    -- Other assumed syntax
    | ExprIfThenElse Expr Expr Expr
    | ExprMatch Expr [(Pattern, Expr)]
    | ExprRepeat Expr [Expr]
    | ExprBinop Operator Expr Expr

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
    -- | Not Negate
    deriving (Show, Eq)

