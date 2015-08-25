import Test.HUnit
import Parser
import Ast

main = runTestTT $ TestList [
    "A simple identifier" ~:
        readExpr "id" ~?= ExprVar "id",

    "A number" ~:
        readExpr "413" ~?= ExprNum 413,

    "A identifier applied to two identifiers." ~:
        readExpr "a(b,c)" ~?= ExprApp (ExprVar "a") [ExprVar "b", ExprVar "c"],

    "An if-then-else with identifiers" ~:
        readExpr "if a then b else c" ~?=
        ExprIfThenElse (ExprVar "a") (ExprVar "b") (ExprVar "c"),

    "A let binding with a resulting expression" ~:
        readExpr "{ a = 2; a }" ~?=
        ExprLetBind "a" (ExprNum 2) (ExprVar "a"),

    "An effect binding with a resulting expression" ~:
        readExpr "{ a <- f(x); g(a) }" ~?=
        ExprEffectBind "a" (ExprApp (ExprVar "f") [ExprVar "x"])
                           (ExprApp (ExprVar "g") [ExprVar "a"])
    ]

