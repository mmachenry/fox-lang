module Primitives where

import Ast

primitives :: [(String, Value)]
primitives = [
      ("test", ValPrimitive "test" (\args->case args of
        [ValNum i] -> Right $ ValNum (i+1)
        _ -> Left $ ErrorGeneric "argument error."
        ))
    , ("print", ValPrimitive "print" (\args->case args of
        [x] -> Right ValUnit
        _ -> Left $ ErrorGeneric "argument error."
        ))
    ]

operators :: [(String, Value)]
operators = [
    ]

