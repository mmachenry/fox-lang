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
        ExprBinOp Add (ExprNum 1) (ExprBinOp Mul (ExprNum 2) (ExprNum 3)),

    "Arithmetic expression: 1 * 2 + 3" ~:
        readExpr "1 * 2 + 3" ~?=
        ExprBinOp Add (ExprBinOp Mul (ExprNum 1) (ExprNum 2)) (ExprNum 3),

    "Arithmetic expression: 1 * (2 + 3) " ~:
        readExpr "1 * (2 + 3)" ~?=
        ExprBinOp Mul (ExprNum 1) (ExprBinOp Add (ExprNum 2) (ExprNum 3)),

    "Boolean expression: 3 < 4 && 5 >= 4 || 1 != 0" ~:
        readExpr "3 < 4 && 5 >= 4 || 1 != 0" ~?=
        ExprBinOp Or (ExprBinOp And (ExprBinOp Lt (ExprNum 3) (ExprNum 4))
                                    (ExprBinOp Gte (ExprNum 5) (ExprNum 4)))
                     (ExprBinOp Ne (ExprNum 1) (ExprNum 0)),

    "Boolean expression: 1 != 0 || 3 < 4 && 5 >= 4" ~:
        readExpr "1 != 0 || 3 < 4 && 5 >= 4" ~?=
        ExprBinOp Or (ExprBinOp Ne (ExprNum 1) (ExprNum 0))
                     (ExprBinOp And (ExprBinOp Lt (ExprNum 3) (ExprNum 4))
                                    (ExprBinOp Gte (ExprNum 5) (ExprNum 4))),

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
                           [ExprBinOp Mul (ExprVar "x") (ExprVar "x")]
            ],

    "Two argument function" ~:
        readModule "add (x,y) { x + y }" ~?=
            Module [
                Definition "add" [Parameter "x" TypeInfered,
                                  Parameter "y" TypeInfered]
                           [ExprBinOp Add (ExprVar "x") (ExprVar "y")]
            ],

    "Two definitions in a module" ~:
        readModule "add (x,y) { x + y } mul (x,y) { x * y }" ~?=
            Module [
                Definition "add" [Parameter "x" TypeInfered,
                                  Parameter "y" TypeInfered]
                           [ExprBinOp Add (ExprVar "x") (ExprVar "y")],
                Definition "mul" [Parameter "x" TypeInfered,
                                  Parameter "y" TypeInfered]
                           [ExprBinOp Mul (ExprVar "x") (ExprVar "y")]
            ],

    "Pattern with value constructor" ~:
         expectRight pattern "cons(x,xx)" ~?=
            PatternApp "cons" [PatternId "x", PatternId "xx"],

    -- Examples from the paper:
    "One argument function with type annotation" ~:
        readModule "sqr (x : int) {x * x}" ~?= Module [
            Definition "sqr" [Parameter "x" (TypeIdentifier "int")] [
                ExprBinOp Mul (ExprVar "x") (ExprVar "x")]
        ],
            
    "Square function with a print statement" ~:
        readModule "sqr (x : int) {print (x); x * x }" ~?= Module [
            Definition "sqr" [Parameter "x" (TypeIdentifier "int")] [
                ExprApp (ExprVar "print") [(ExprVar "x")],
                ExprBinOp Mul (ExprVar "x") (ExprVar "x")]
        ],
            
    "Function with parametric type annotations" ~:
        readModule "add (x : 'a, y : 'a) { x + y }" ~?= Module [
            Definition "add" [Parameter "x" (TypeVar "a"), Parameter "y" (TypeVar "a")] [
                ExprBinOp Add (ExprVar "x") (ExprVar "y")]
        ],

    "apply: a higher-order function with type annotations" ~:
        readModule "apply (f : 'a -> 'b, x : 'a) { f(x) }" ~?= Module [
            Definition "apply" [Parameter "f" (TypeFunction [TypeVar "a"] EffectInfered (TypeVar "b")),
                                Parameter "x" (TypeVar "a")]
                [ExprApp (ExprVar "f") [(ExprVar "x")]]
        ],

    "Twice: a higher-order function with type annotation" ~:
        readModule "twice (f : 'a -> partial 'a, x : 'a) { f(f(x)) }" ~?= Module [
            Definition "twice" [
                Parameter "f" (TypeFunction [TypeVar "a"]
                                            EffectPartial
                                            (TypeVar "a")),
                Parameter "x" (TypeVar "a")
                ] [
                ExprApp (ExprVar "f") [ExprApp (ExprVar "f") [ExprVar "x"]]
                ]
            ],

    "Effect bind" ~:
        expectRight statement "f1 <- newref (1)" ~?=
            ExprEffectBind "f1" (ExprApp (ExprVar "newref") [ExprNum 1]),

    "Dereference" ~:
        readExpr "!f1" ~?= ExprUnaryOp Dereference (ExprVar "f1"),

    "Fib example from paper" ~:
        readModule fibExample ~?= Module [
            Definition "fib" [Parameter "n" TypeInfered] [
                ExprEffectBind "f1" (ExprApp (ExprVar "newref") [ExprNum 1]),
                ExprEffectBind "f2" (ExprApp (ExprVar "newref") [ExprNum 1]),
                ExprRepeat (ExprBinOp Sub (ExprVar "n") (ExprNum 1)) [
                    ExprEffectBind "sum" (ExprBinOp Add (ExprUnaryOp Dereference (ExprVar "f1")) (ExprUnaryOp Dereference (ExprVar "f2"))),
                    ExprBinOp Assign (ExprVar "f1") (ExprUnaryOp Dereference (ExprVar "f2")),
                    ExprBinOp Assign (ExprVar "f2") (ExprVar "sum")
                ],
                ExprUnaryOp Dereference (ExprVar "f2")]],

    "Sum line of fib" ~:
        expectRight statement "sum <- !f1 + !f2" ~?=
            ExprEffectBind "sum"
                (ExprBinOp Add (ExprUnaryOp Dereference (ExprVar "f1"))
                               (ExprUnaryOp Dereference (ExprVar "f2"))),

    "Double deref" ~:
        expectRight expr "!f1 + !f2" ~?=
                ExprBinOp Add (ExprUnaryOp Dereference (ExprVar "f1"))
                              (ExprUnaryOp Dereference (ExprVar "f2")),

    "Map example from paper" ~:
        readModule mapExample ~?= Module [
            Definition "map" [Parameter "f" TypeInfered,
                              Parameter "xs" TypeInfered] [
                ExprMatch (ExprVar "xs") [
                    (PatternApp "cons" [PatternId "x",PatternId "xx"],
                        ExprStatementBlock [
                            ExprEffectBind "y" (ExprApp (ExprVar "f") [ExprVar "x"]),
                            ExprEffectBind "yy" (ExprApp (ExprVar "map") [ExprVar "f",ExprVar "xx"]),
                            ExprApp (ExprVar "cons") [ExprVar "y",ExprVar "yy"]
                        ]),
                    (PatternId "nil",ExprVar "nil")
                    ]
                ]
            ]
    ]

