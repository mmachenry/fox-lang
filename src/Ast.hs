module Ast (
    Module(..),
    Definition(..),
    Parameter(..),
    Pattern(..),
    Expr(..),
    BinOp(..),
    UnaryOp(..),
    Type(..),
    Effect(..),
    ) where

type Identifier = String

data Type =
      TypeInferred
    | TypeIdentifier Identifier
    | TypeForAll Identifier Type
    | TypeVar Identifier
    | TypeFunction [Type] Effect Type
    deriving (Show, Eq)

data Effect =
      EffectInferred
    | EffectTotal
    | EffectPartial
    | EffectDivergent
    | EffectPure
    deriving (Show, Eq)

data Module = Module [Definition] deriving (Eq, Show)

data Definition = Definition Identifier [Parameter] [Expr] deriving (Eq, Show)

data Parameter = Parameter Identifier Type deriving (Eq, Show)

data Pattern =
      PatternId Identifier
    | PatternApp Identifier [Pattern]
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
    | ExprStatementBlock [Expr]
    | ExprBinOp BinOp Expr Expr
    | ExprUnaryOp UnaryOp Expr

    -- References
    | ExprAlloc Expr
    | ExprRead Expr
    | ExprWrite Expr Expr

    -- Literal
    | ExprNum Integer

    deriving (Show, Eq)

data BinOp =
      Add | Sub | Mul | Div
    | BoolEq | Ne | Gt | Lt | Gte | Lte
    | And | Or
    | Assign
    deriving (Show, Eq)

data UnaryOp = Dereference deriving (Show, Eq)

