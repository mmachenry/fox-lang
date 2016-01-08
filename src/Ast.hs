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
    beginRun,
    endRun,
    assignValue,
    getValue,
    allocateReference,
    ) where

import Data.Ratio
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Vector.Persistent as V

--------------
-- State
--------------
type FoxState = V.Vector Heap
type Heap = V.Vector Value
type HeapId = Int
type ReferenceId = Int

emptyState :: FoxState
emptyState = V.singleton V.empty

beginRun :: EvalMonad ()
beginRun = modify (flip V.snoc V.empty)

endRun :: EvalMonad ()
endRun = do
    state <- get
    let newState = V.take (V.length state - 1) state
    put newState

allocateReference :: Value -> EvalMonad (HeapId, ReferenceId)
allocateReference val = do
    state <- get
    let topHeapId = V.length state - 1
    let topHeap = V.unsafeIndex state topHeapId
    let newState = V.update topHeapId (V.snoc topHeap val) state
    put newState
    return (topHeapId, V.length topHeap)

assignValue :: (HeapId, ReferenceId) -> Value -> EvalMonad ()
assignValue (heapid, refid) val = do
    state <- get
    let heap = V.unsafeIndex state heapid
    let newHeap = V.update refid val heap
    let newState = V.update heapid newHeap state
    put newState

getValue :: (HeapId, ReferenceId) -> EvalMonad Value
getValue (heapid, refid) = do
    state <- get
    let heap = V.unsafeIndex state heapid
    return $ V.unsafeIndex heap refid

--------------
-- Eval Monad
--------------
type EvalMonad = ExceptT FoxError (StateT FoxState IO)

runEval :: EvalMonad a -> IO (Either FoxError a)
runEval e = do
    (value, _resultState) <- runStateT (runExceptT e) emptyState
    return value

----------------
-- Rest
----------------
type FoxNum = Ratio Integer

data Value =
      ValUnit
    | ValNum FoxNum
    | ValBool Bool
    | ValRef (HeapId, ReferenceId)
    | ValClosure Env [Parameter] Expr
    | ValPrimitive String ([Value] -> EvalMonad Value)

instance Show Value where
    show ValUnit = "<unit>"
    show (ValNum n) = show n
    show (ValBool b) = show b
    show ValClosure{} = "<func>"
    show (ValPrimitive name _) = "<primitive:" ++ name ++ ">"
    show (ValRef _) = "<reference>"

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

