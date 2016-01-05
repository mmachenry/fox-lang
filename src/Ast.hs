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
    FoxError(..),
    Env,
    FoxNum,
    EvalMonad,
    runEval,
    throwError,
    getNextRefId,
    assignValue,
    getValue,
    ) where

import Data.Ratio
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

--------------
-- Eval Monad
--------------
type ReferenceId = Int
type Heap = Map.Map ReferenceId Value

type EvalMonad = ExceptT FoxError (StateT Heap IO)

runEval :: EvalMonad a -> IO (Either FoxError a)
runEval e = do
    (value, _resultState) <- runStateT (runExceptT e) emptyHeap
    return value

emptyHeap :: Heap
emptyHeap = Map.empty

getNextRefId :: EvalMonad ReferenceId
getNextRefId = do
    m <- get
    if Map.size m > 0
    then let (maxid, _value) = Map.findMax m
         in return (maxid + 1)
    else return 0

assignValue :: ReferenceId -> Value -> EvalMonad ()
assignValue refId val = modify (Map.insert refId val)

getValue :: ReferenceId-> EvalMonad Value
getValue refId = do
    m <- get
    let mValue = Map.lookup refId m
    case mValue of
        Nothing -> throwError $ DynamicError "Unbound reference"
        Just v -> return v

----------------
-- Rest
----------------
type FoxNum = Ratio Integer

data Value =
      ValUnit
    | ValNum FoxNum
    | ValBool Bool
    | ValRef ReferenceId
    | ValClosure Env [Parameter] Expr
    | ValPrimitive String ([Value] -> EvalMonad Value)

instance Show Value where
    show ValUnit = "<unit>"
    show (ValNum n) = show n
    show (ValBool b) = show b
    show ValClosure{} = "<func>"
    show (ValPrimitive name _) = "<primitive:" ++ name ++ ">"

instance Eq Value where
    (ValNum n1) == (ValNum n2) = n1 == n2
    (ValBool b1) == (ValBool b2) = b1 == b2
    ValClosure{} == ValClosure{} = False
    (ValPrimitive name1 _) == (ValPrimitive name2 _) = name1 == name2
    ValUnit == ValUnit = True
    _ == _ = False

data FoxError =
      DynamicError String
    | StaticError String
    | TypeError String
    | ParserError String -- consider making this Parsec.ParseError
    | UserError Value
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
    | ExprNum FoxNum

    deriving (Show, Eq)

