import Eval
import Ast
import Parser
import System.Environment

main = do
    [filename] <- getArgs
    result <- readProgramFile filename
    case result of
        Left e -> print e
        Right p -> do
            value <- evalModule p
            case value of
                Left e -> print e
                Right v -> print v

