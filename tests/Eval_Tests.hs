import Test.HUnit
import Parser
import Ast
import Eval

main = runTestTT $ TestList [
    "A number" ~:
        evalExpr [] (ExprNum 413) ~?= Right (ValNum 413)

    , "Bound vqriable" ~:
        evalExpr [("i", ValNum 413)] (ExprVar "i") ~?= Right (ValNum 413)
    ]

