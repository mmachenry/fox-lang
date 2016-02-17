module Ast (
    Module(..),
    Definition(..),
    Parameter(..),
    Pattern(..),
    Expr(..),
    Type(..),
    Effect(..),
    FoxNum,
    FoxValue(..),
    EvalMonad,
    runEval,
    ) where

import Data.Ratio
import qualified Data.Map as Map
import State
import InterpM
import Env
import Exn

type EvalMonad = InterpM (FoxEnv FoxValue) FoxExn (FoxState FoxValue)
runEval = runInterpM emptyEnv emptyState

type FoxNum = Ratio Integer

data FoxValue =
      ValUnit
    | ValNum FoxNum
    | ValBool Bool
    | ValRef ReferenceId
    | ValClosure (FoxEnv FoxValue) [Parameter] Expr
    | ValPrimitive String ([FoxValue] -> EvalMonad FoxValue)

instance Show FoxValue where
    show ValUnit = "<unit>"
    show (ValNum n) = show n
    show (ValBool b) = show b
    show ValClosure{} = "<func>"
    show (ValPrimitive name _) = "<primitive:" ++ name ++ ">"
    show (ValRef _) = "<reference>"

instance Eq FoxValue where
    (ValNum n1) == (ValNum n2) = n1 == n2
    (ValBool b1) == (ValBool b2) = b1 == b2
    ValClosure{} == ValClosure{} = False
    (ValPrimitive name1 _) == (ValPrimitive name2 _) = name1 == name2
    ValUnit == ValUnit = True
    _ == _ = False

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
    | ExprLiteral FoxValue

    deriving (Show, Eq)

