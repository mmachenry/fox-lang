module Env (
    Identifier,
    FoxEnv,
    emptyEnv,
    ) where

import qualified Data.Map as Map

type Identifier = String

type FoxEnv val = Map.Map Identifier val

emptyEnv = Map.empty

