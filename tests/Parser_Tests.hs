import Test.HUnit
import Parser
import Ast

main = runTestTT $ TestList [
    readExpr "id" ~?= ExprVar "id"
    , readExpr "413" ~?= ExprNum 413
    , readExpr "a(b,c)" ~?= ExprApp (ExprVar "a") [ExprVar "b", ExprVar "c"]
    ]

