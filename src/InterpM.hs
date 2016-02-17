module InterpM (
    InterpM,
    runInterpM,
    throwError,
    ask,
    get,
    put,
    modify,
    local,
    liftIO,
    ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class

-- type FoxInterpM (FoxEnv Value) FoxError (FoxState Value)
type InterpM env exn st = ReaderT env (ExceptT exn (StateT st IO))

runInterpM :: env -> st -> InterpM env exn st val -> IO (Either exn val)
runInterpM initEnv initState interp = do
    (value, _resultState) <-
        runStateT (runExceptT (runReaderT interp initEnv)) initState
    return value

