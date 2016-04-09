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

-- Inspired by http://web.cecs.pdx.edu/~mpj/pubs/modinterp.html

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.IO.Class

type InterpM env exn st val =
    ReaderT env (ExceptT exn (StateT st (ContT (Either exn val, st) IO)))

runInterpM :: env -> st -> InterpM env exn st val val -> IO (Either exn val)
runInterpM initEnv initState interp = do
    (value, _resultState) <-
        runContT (runStateT (runExceptT (runReaderT interp initEnv)) initState) return
    return value

