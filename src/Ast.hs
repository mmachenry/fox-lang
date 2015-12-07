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
    Identifier,
    Value(..),
    Error(..),
    Env
    ) where

data Value =
      ValNum Integer
    | ValBool Bool
    | ValClosure Env [Parameter] Expr
    | ValPrimitive String ([Value] -> Either Error Value)

instance Show Value where
    show (ValNum i) = show i
    show (ValBool b) = show b
    show (ValClosure _ _ _) = "<func>"
    show (ValPrimitive name _) = "<primitive:" ++ name ++ ">"

instance Eq Value where
    (ValNum i1) == (ValNum i2) = i1 == i2
    (ValBool b1) == (ValBool b2) = b1 == b2
    (ValClosure _ _ _) == (ValClosure _ _ _) = False
    (ValPrimitive name1 _) == (ValPrimitive name2 _) = name1 == name2
    _ == _ = False

data Error =
      ErrorGeneric String
    deriving (Eq, Show)

type Env = [(Identifier, Value)]

type Identifier = String

data Parameter = Parameter {
    parameterIdentifier :: Identifier,
    parameterType :: Type
    } deriving (Eq, Show)

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

data Definition = Definition Identifier [Parameter] Expr deriving (Eq, Show)

data Pattern =
      PatternId Identifier
    | PatternApp Identifier [Pattern]
    deriving (Eq, Show)

data Expr =
    -- Core syntax
      ExprVar Identifier
    | ExprApp Expr [Expr]
    | ExprAbs [Parameter] Expr
    | ExprLetBind Identifier Expr Expr
    | ExprEffectBind Identifier Expr Expr
    | ExprRun Expr

    -- Other assumed syntax
    | ExprCompound Expr Expr
    | ExprIfThenElse Expr Expr Expr
    | ExprMatch Expr [(Pattern, Expr)]
    | ExprRepeat Expr Expr
    | ExprBinOp BinOp Expr Expr
    | ExprUnaryOp UnaryOp Expr

    -- References
    | ExprAlloc Expr
    | ExprRead Expr
    | ExprWrite Expr Expr

    -- Literal
    | ExprNum Integer
    | ExprBool Bool

    deriving (Show, Eq)

data BinOp =
      Add | Sub | Mul | Div
    | BoolEq | Ne | Gt | Lt | Gte | Lte
    | And | Or
    -- FIXME Should not have assign operator and aso ExprWrite
    | Assign
    deriving (Show, Eq)

-- FIXME Should not have dereference op and also ExprRead
data UnaryOp = Dereference deriving (Show, Eq)

