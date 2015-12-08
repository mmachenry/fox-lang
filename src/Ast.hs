module Ast (
    Module(..),
    Definition(..),
    Parameter(..),
    Pattern(..),
    Expr(..),
    Type(..),
    Effect(..),
    Identifier,
    Value(..),
    Error(..),
    Env
    ) where

data Value =
      ValUnit
    | ValNum Integer
    | ValBool Bool
    | ValClosure Env [Parameter] Expr
    | ValPrimitive String ([Value] -> Either Error Value)

instance Show Value where
    show ValUnit = "<unit>"
    show (ValNum i) = show i
    show (ValBool b) = show b
    show ValClosure{} = "<func>"
    show (ValPrimitive name _) = "<primitive:" ++ name ++ ">"

instance Eq Value where
    (ValNum i1) == (ValNum i2) = i1 == i2
    (ValBool b1) == (ValBool b2) = b1 == b2
    ValClosure{} == ValClosure{} = False
    (ValPrimitive name1 _) == (ValPrimitive name2 _) = name1 == name2
    ValUnit == ValUnit = True
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

    -- Literal
    | ExprNum Integer

    deriving (Show, Eq)

