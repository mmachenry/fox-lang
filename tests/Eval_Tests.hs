import Test.HUnit
import Parser
import Ast
import Eval

main = runTestTT $ TestList [
    "A number" ~:
        evalExpr (ExprNum 413) ~?= Right (ValNum 413)
    ]

