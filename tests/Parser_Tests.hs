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

fibExample =
    "\
    \    fib(n){\
    \        f1 <- newref (1);\
    \        f2 <- newref (1);\
    \        repeat (n - 1) {\
    \            sum <- !f1 + !f2;\
    \            f1 := !f2;\
    \            f2 := sum\
    \        } ;\
    \        !f2 \
    \     } "

mapExample =
    "\
    \    map(f,xs) {\
    \        match (xs) {\
    \            cons(x,xx) -> { y <- f(x);\
    \                            yy <- map(f,xx);\
    \                            cons(y,yy) };\
    \            nil -> nil\
    \        }\
    \    } "

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
        ExprApp (ExprVar "+") [ExprNum 1, ExprApp (ExprVar "*") [ExprNum 2, ExprNum 3]],

    "Arithmetic expression: 1 * 2 + 3" ~:
        readExpr "1 * 2 + 3" ~?=
        ExprApp (ExprVar "+") [ExprApp (ExprVar "*") [ExprNum 1, ExprNum 2], ExprNum 3],

    "Arithmetic expression: 1 * (2 + 3) " ~:
        readExpr "1 * (2 + 3)" ~?=
        ExprApp (ExprVar "*") [ExprNum 1, ExprApp (ExprVar "+") [ExprNum 2, ExprNum 3]],

    "Boolean expression: 3 < 4 && 5 >= 4 || 1 != 0" ~:
        readExpr "3 < 4 && 5 >= 4 || 1 != 0" ~?=
        ExprApp (ExprVar "||") [
            ExprApp (ExprVar "&&") [
                ExprApp (ExprVar "<") [ExprNum 3, ExprNum 4],
                ExprApp (ExprVar ">=") [ExprNum 5, ExprNum 4]],
            ExprApp (ExprVar "!=") [ExprNum 1, ExprNum 0]],

    "Boolean expression: 1 != 0 || 3 < 4 && 5 >= 4" ~:
        readExpr "1 != 0 || 3 < 4 && 5 >= 4" ~?=
        ExprApp (ExprVar "||") [
            ExprApp (ExprVar "!=") [ExprNum 1, ExprNum 0],
            ExprApp (ExprVar "&&") [
                ExprApp (ExprVar "<") [ExprNum 3, ExprNum 4],
                ExprApp (ExprVar ">=") [ExprNum 5, ExprNum 4]]],

    "Simple module" ~:
        readModule "square (x) { x * x }" ~?=
            Module [
                Definition "square" [Parameter "x" TypeInferred]
                    (ExprApp (ExprVar "*") [ExprVar "x", ExprVar "x"])
            ],

    "Two argument function" ~:
        readModule "add (x,y) { x + y }" ~?=
            Module [
                Definition "add" [Parameter "x" TypeInferred,
                                  Parameter "y" TypeInferred]
                    (ExprApp (ExprVar "+") [ExprVar "x", ExprVar "y"])
            ],

    "Two definitions in a module" ~:
        readModule "add (x,y) { x + y } mul (x,y) { x * y }" ~?=
            Module [
                Definition "add" [Parameter "x" TypeInferred,
                                  Parameter "y" TypeInferred]
                    (ExprApp (ExprVar "+") [ExprVar "x", ExprVar "y"]),
                Definition "mul" [Parameter "x" TypeInferred,
                                  Parameter "y" TypeInferred]
                    (ExprApp (ExprVar "*") [ExprVar "x", ExprVar "y"])
            ],

    "Pattern with value constructor" ~:
         expectRight pattern "cons(x,xx)" ~?=
            PatternApp "cons" [PatternId "x", PatternId "xx"],

    -- Examples from the paper:
    "One argument function with type annotation" ~:
        readModule "sqr (x : int) {x * x}" ~?= Module [
            Definition "sqr" [Parameter "x" (TypeIdentifier "int")] (
                ExprApp (ExprVar "*") [ExprVar "x", ExprVar "x"])
        ],

    "Square function with a print statement" ~:
        readModule "sqr (x : int) {print (x); x * x }" ~?= Module [
            Definition "sqr" [Parameter "x" (TypeIdentifier "int")]
                $ ExprCompound (ExprApp (ExprVar "print") [(ExprVar "x")])
                              (ExprApp (ExprVar "*")
                                       [ExprVar "x", ExprVar "x"])
        ],
            
    "Function with parametric type annotations" ~:
        readModule "add (x : 'a, y : 'a) { x + y }" ~?= Module [
            Definition "add" [Parameter "x" (TypeVar "a"),
                              Parameter "y" (TypeVar "a")]
                (ExprApp (ExprVar "+") [ExprVar "x", ExprVar "y"])
        ],

    "apply: a higher-order function with type annotations" ~:
        readModule "apply (f : 'a -> 'b, x : 'a) { f(x) }" ~?= Module [
            Definition "apply" [Parameter "f" (TypeFunction [TypeVar "a"] EffectInferred (TypeVar "b")),
                                Parameter "x" (TypeVar "a")]
                (ExprApp (ExprVar "f") [(ExprVar "x")])
        ],

    "Twice: a higher-order function with type annotation" ~:
        readModule "twice (f : 'a -> partial 'a, x : 'a) { f(f(x)) }" ~?= Module [
            Definition "twice" [
                Parameter "f" (TypeFunction [TypeVar "a"]
                                            EffectPartial
                                            (TypeVar "a")),
                Parameter "x" (TypeVar "a")
                ]
                (ExprApp (ExprVar "f") [ExprApp (ExprVar "f") [ExprVar "x"]])
            ],

    "Effect bind" ~:
        expectRight expr "{ f1 <- newref (1); 1 }" ~?=
            ExprEffectBind "f1" (ExprApp (ExprVar "newref") [ExprNum 1]) (ExprNum 1),

    "Dereference" ~:
        readExpr "!f1" ~?= ExprApp (ExprVar "!") [ExprVar "f1"],

    "Fib example from paper" ~:
        readModule fibExample ~?= Module [
            Definition "fib" [Parameter "n" TypeInferred] $
                ExprEffectBind "f1" (ExprApp (ExprVar "newref") [ExprNum 1]) (
                    ExprEffectBind "f2" (ExprApp (ExprVar "newref") [ExprNum 1]) (
                        ExprCompound (
                            ExprRepeat (ExprApp (ExprVar "-") [ExprVar "n", ExprNum 1])
                                (ExprEffectBind "sum"
                                    (ExprApp (ExprVar "+") [
                                        ExprApp (ExprVar "!") [ExprVar "f1"],
                                        ExprApp (ExprVar "!") [ExprVar "f2"]]) (
                                    ExprCompound 
                                        (ExprApp (ExprVar ":=") [
                                            ExprVar "f1",
                                            ExprApp (ExprVar "!") [ExprVar "f2"]]) (
                                        (ExprApp (ExprVar ":=") [ExprVar "f2", ExprVar "sum"])))))
                        (ExprApp (ExprVar "!") [ExprVar "f2"])))
        ],

{-
    "Sum line of fib" ~:
        expectRight expr "{ sum <- !f1 + !f2; 1 }" ~?=
            ExprEffectBind "sum"
                (ExprBinOp Add (ExprUnaryOp Dereference (ExprVar "f1"))
                               (ExprUnaryOp Dereference (ExprVar "f2")))
                (ExprNum 1),

    "Double deref" ~:
        expectRight expr "!f1 + !f2" ~?=
                ExprBinOp Add (ExprUnaryOp Dereference (ExprVar "f1"))
                              (ExprUnaryOp Dereference (ExprVar "f2")),

-}

    "Map example from paper" ~:
        readModule mapExample ~?= Module [
            Definition "map" [Parameter "f" TypeInferred,
                              Parameter "xs" TypeInferred] (
                ExprMatch (ExprVar "xs") [
                    (PatternApp "cons" [PatternId "x",PatternId "xx"],
                        ExprEffectBind "y" (ExprApp (ExprVar "f") [ExprVar "x"])
                            $ ExprEffectBind "yy" (ExprApp (ExprVar "map") [ExprVar "f",ExprVar "xx"])
                            $ ExprApp (ExprVar "cons") [ExprVar "y",ExprVar "yy"]),
                    (PatternId "nil",ExprVar "nil")
                    ]
                )
            ]
    ]

