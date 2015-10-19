module Ast (
    Module(..),
    Definition(..),
    Parameter(..),
    Pattern(..),
    Expr(..),
    BinOp(..),
    UnaryOp(..),
    Type(..),
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
    | ExprApp Expr [Expr]
    | ExprAbs [Parameter] Expr
    | ExprLetBind Identifier Expr
    | ExprEffectBind Identifier Expr
    | ExprRun [Expr]

    -- Other assumed syntax
    | ExprIfThenElse Expr Expr Expr
    | ExprMatch Expr [(Pattern, Expr)]
    | ExprRepeat Expr [Expr]
    | ExprBinOp BinOp Expr Expr
    | ExprUnaryOp UnaryOp Expr

    -- References
    | ExprAlloc Expr
    | ExprRead Expr
    | ExprWrite Expr Expr

    -- Literal
    | ExprNum Integer
    | ExprTuple [Expr]
    | ExprNil -- likely this is just defined in language as an AST
    deriving (Show, Eq)

data BinOp =
      Add | Sub | Mul | Div
    | BoolEq | Ne | Gt | Lt | Gte | Lte
    | And | Or
    | Assign
    deriving (Show, Eq)

data UnaryOp = Dereference deriving (Show, Eq)

