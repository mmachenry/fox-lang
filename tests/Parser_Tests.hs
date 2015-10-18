import Test.HUnit
import Parser
import Ast

expectRight parser str = case readStr parser str of
    Left err -> error (show err)
    Right val -> val

expectLeft parser str = case readStr parser str of
    Left err -> show err
    Right _ -> error "No parse error."

readExpr = expectRight expr
readModule = expectRight definitions

main = runTestTT $ TestList [
    "A number" ~:
        readExpr "413" ~?= ExprNum 413,

    "A simple identifier" ~:
        readExpr "id" ~?= ExprVar "id",

    "f applied to x " ~:
        readExpr "f(x)" ~?= ExprApp (ExprVar "f") [(ExprVar "x")],

    "A identifier applied to a 2-tuple." ~:
        readExpr "a(b,c)" ~?=
            ExprApp (ExprVar "a") [ExprVar "b", ExprVar "c"],

    "An if-then-else with identifiers" ~:
        readExpr "if a then b else c" ~?=
        ExprIfThenElse (ExprVar "a") (ExprVar "b") (ExprVar "c"),

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
                                    (ExprBinop Gte (ExprNum 5) (ExprNum 4))),

    -- I'm undecided on whether this should be a parser error or discovered in
    -- a static pass of the AST.
    "Let at the end of a block" ~:
        expectRight definitions "f(x) { x = 5 }" ~?= Module [
            Definition "f" [Parameter "x" TypeInfered]
                [ExprLetBind "x" (ExprNum 5)]],

    "Simple module" ~:
        readModule "square (x) { x * x }" ~?=
            Module [
                Definition "square" [Parameter "x" TypeInfered]
                           [ExprBinop Mul (ExprVar "x") (ExprVar "x")]
            ],

    "Two argument function" ~:
        readModule "add (x,y) { x + y }" ~?=
            Module [
                Definition "add" [Parameter "x" TypeInfered,
                                  Parameter "y" TypeInfered]
                           [ExprBinop Add (ExprVar "x") (ExprVar "y")]
            ],

    "Two definitions in a module" ~:
        readModule "add (x,y) { x + y } mul (x,y) { x * y }" ~?=
            Module [
                Definition "add" [Parameter "x" TypeInfered,
                                  Parameter "y" TypeInfered]
                           [ExprBinop Add (ExprVar "x") (ExprVar "y")],
                Definition "mul" [Parameter "x" TypeInfered,
                                  Parameter "y" TypeInfered]
                           [ExprBinop Mul (ExprVar "x") (ExprVar "y")]
            ],

    "Pattern with value constructor" ~:
         readStr pattern "cons(x,xx)" ~?= Right (PatternId "d"),

    -- Examples from the paper:
    "One argument function with type annotation" ~:
        readModule "sqr (x : int) {x * x}" ~?= Module [
            Definition "sqr" [Parameter "x" TypeInt] [
                ExprBinop Mul (ExprVar "x") (ExprVar "x")]
        ],
            
    "Square function with a print statement" ~:
        readModule "sqr (x : int) {print (x); x * x}" ~?= Module [
            Definition "sqr" [Parameter "x" TypeInt] [
                ExprApp (ExprVar "print") [(ExprVar "x")],
                ExprBinop Mul (ExprVar "x") (ExprVar "x")]
        ],
            
    --twice (f : a -> partial a, x : a) { f(f(x)) }

    "Effect bind" ~:
        readExpr "f1 <- newref (1)" ~?=
            ExprEffectBind "f1" (ExprApp (ExprVar "newref") [ExprNum 1]),

    "Fib example from papper" ~:
        readModule "\
        \    fib(n){\
        \        f1 <- newref (1);\
        \        f2 <- newref (1);\
        \        repeat (n - 1) {\
        \            sum <- !f1 + !f2;\
        \            f1 := !f2;\
        \            f2 := sum;\
        \        }\
        \        !f2 ;\
        \     } " ~?=
        Module [],

    "Map example from paper" ~:
        readModule "\
        \    map(f,xs) {\
        \        match (xs) {\
        \            cons(x,xx) -> { y <- f(x);\
        \                            yy <- map(f,xx);\
        \                            cons(y,yy) }\
        \            nil -> nil\
        \        }\
        \    } " ~?=
        Module []
    ]

