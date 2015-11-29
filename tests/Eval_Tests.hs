import Test.HUnit
import Parser
import Ast
import Eval

main = runTestTT $ TestList [
    "A number" ~:
        evalExpr [] (ExprNum 413) ~?= Right (ValNum 413)

    , "Bound vqriable" ~:
        evalExpr [("i", ValNum 413)] (ExprVar "i") ~?= Right (ValNum 413)

    , "Simple if expression with true test." ~:
        evalExpr [] (ExprIfThenElse (ExprBool True) (ExprNum 413) (ExprNum 2))
            ~?= Right (ValNum 413)

    , "Simple if expression with false test." ~:
        evalExpr [] (ExprIfThenElse (ExprBool False) (ExprNum 2) (ExprNum 413))
            ~?= Right (ValNum 413)
    ]

