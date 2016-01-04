import Eval
import Ast
import Parser
import System.Environment

main = do
    [filename] <- getArgs
    result <- readProgramFile filename
    case result >>= evalModule of
        Left e -> print e
        Right v -> print v

