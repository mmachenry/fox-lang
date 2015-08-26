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
        readExpr "{a = 2; a }" ~?=
        ExprLetBind "a" (ExprNum 2) (ExprVar "a"),

    "An effect binding with a resulting expression" ~:
        readExpr "{a<-f(x);g(a)}" ~?=
        ExprEffectBind "a" (ExprApp (ExprVar "f") [ExprVar "x"])
                           (ExprApp (ExprVar "g") [ExprVar "a"]),

    "Arithmetic expression: 1 + 2 * 3" ~:
        readExpr "1 + 2 * 3" ~?=
        ExprBinop Add (ExprNum 1) (ExprBinop Mul (ExprNum 2) (ExprNum 3)),

    "Arithmetic expression: 1 * 2 + 3" ~:
        readExpr "1 * 2 + 3" ~?=
        ExprBinop Add (ExprBinop Mul (ExprNum 1) (ExprNum 2)) (ExprNum 3),

    "Arithmetic expression: 1 * (2 + 3) " ~:
        readExpr "1 * (2 + 3)" ~?=
        ExprBinop Mul (ExprNum 1) (ExprBinop Add (ExprNum 2) (ExprNum 3)),

    "Boolean expression: 3 < 4 && 5 >= 4 || 1 != 0" ~:
        readExpr "3 < 4 && 5 >= 4 || 1 != 0" ~?=
        ExprBinop Or (ExprBinop And (ExprBinop Lt (ExprNum 3) (ExprNum 4))
                                    (ExprBinop Gte (ExprNum 5) (ExprNum 4)))
                     (ExprBinop Ne (ExprNum 1) (ExprNum 0)),

    "Boolean expression: 1 != 0 || 3 < 4 && 5 >= 4" ~:
        readExpr "1 != 0 || 3 < 4 && 5 >= 4" ~?=
        ExprBinop Or (ExprBinop Ne (ExprNum 1) (ExprNum 0))
                     (ExprBinop And (ExprBinop Lt (ExprNum 3) (ExprNum 4))
                                    (ExprBinop Gte (ExprNum 5) (ExprNum 4)))
    ]

