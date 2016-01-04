import Test.HUnit
import Parser
import Ast
import Eval
import Data.Ratio

main = runTestTT $ TestList [
    "A number" ~:
        evalExpr [] (ExprNum 413) ~?= Right (ValNum 413)

    , "Call test" ~:
        evalExpr [] (ExprApp (ExprVar "test") [ExprNum 412])
            ~?= Right (ValNum 413)

    , "Bound vqriable" ~:
        evalExpr [("i", ValNum 413)] (ExprVar "i")
            ~?= Right (ValNum 413)

    , "Apply a lambda to an argument" ~:
        evalExpr [] (ExprApp (ExprAbs [Parameter "x" TypeInferred]
                             (ExprVar "x")) [ExprNum 413])
            ~?= Right (ValNum 413)

    , "Apply curried lambda to two arguments" ~:
        evalExpr [] (ExprApp (ExprApp
                        (ExprAbs [Parameter "x" TypeInferred]
                                 (ExprAbs [Parameter "y" TypeInferred]
                                          (ExprApp (ExprVar "+") [ExprVar "x", ExprVar "y"])))
                        [ExprNum 413]) 
                        [ExprNum 0])
            ~?= Right (ValNum 413)

    , "Simple if expression with true test." ~:
        evalExpr [] (ExprIfThenElse (ExprVar "true") (ExprNum 413) (ExprNum 2))
            ~?= Right (ValNum 413)

    , "Simple if expression with false test." ~:
        evalExpr [] (ExprIfThenElse (ExprVar "false") (ExprNum 2) (ExprNum 413))
            ~?= Right (ValNum 413)

    , "Add operator" ~:
        evalExpr [] (ExprApp (ExprVar "+") [ExprNum 412, ExprNum 1])
            ~?= Right (ValNum 413)

    , "Sub operator" ~:
        evalExpr [] (ExprApp (ExprVar "-") [ExprNum 414, ExprNum 1])
            ~?= Right (ValNum 413)

    , "Mul operator" ~:
        evalExpr [] (ExprApp (ExprVar "*") [ExprNum 7, ExprNum 59])
            ~?= Right (ValNum 413)

    , "Div operator" ~:
        evalExpr [] (ExprApp (ExprVar "/") [ExprNum 826, ExprNum 2])
            ~?= Right (ValNum 413)

    , "Noninteger division" ~:
        evalExpr [] (ExprApp (ExprVar "/") [ExprNum 44, ExprNum 14])
            ~?= Right (ValNum (22%7))

    , "Equal operator" ~:
        evalExpr [] (ExprApp (ExprVar "==") [ExprNum 413, ExprNum 2])
            ~?= Right (ValBool False)

    , "Not equal operator" ~:
        evalExpr [] (ExprApp (ExprVar "!=") [ExprNum 413, ExprNum 2])
            ~?= Right (ValBool True)

    , "Gt operator" ~:
        evalExpr [] (ExprApp (ExprVar ">") [ExprNum 413, ExprNum 2])
            ~?= Right (ValBool True)

    , "Lt operator" ~:
        evalExpr [] (ExprApp (ExprVar "<") [ExprNum 413, ExprNum 2])
            ~?= Right (ValBool False)

    , "Gte operator" ~:
        evalExpr [] (ExprApp (ExprVar ">=") [ExprNum 413, ExprNum 413])
            ~?= Right (ValBool True)

    , "Lte operator" ~:
        evalExpr [] (ExprApp (ExprVar "<=") [ExprNum 413, ExprNum 2])
            ~?= Right (ValBool False)

    , "And operator" ~:
        evalExpr [] (ExprApp (ExprVar "&&") [ExprVar "true", ExprVar "false"])
            ~?= Right (ValBool False)

    , "And operator" ~:
        evalExpr [] (ExprApp (ExprVar "||") [ExprVar "true", ExprVar "false"])
            ~?= Right (ValBool True)

    , "Simple run, allocate, write, read block." ~:
        evalExpr [] (ExprRun
                (ExprEffectBind "x" (ExprApp (ExprVar "newref") [ExprNum 2])
                    (ExprCompound
                        (ExprApp (ExprVar ":=") [ExprVar "x", ExprNum 413])
                        (ExprApp (ExprVar "!") [ExprVar "x"])))
            ) ~?= Right (ValNum 413)

    , "Let bind expression { x = 413; x} " ~:
        evalExpr [] (ExprLetBind "x" (ExprNum 413) (ExprVar "x"))
            ~?= Right (ValNum 413)

    , "Compound expression { print 2; 413 }" ~:
        evalExpr [] (ExprCompound (ExprApp (ExprVar "print") [ExprNum 2])
                                  (ExprNum 413))
            ~?= Right (ValNum 413)

    , "Effect bind expression { u <- print 2; u }" ~:
        evalExpr [] (ExprEffectBind "u" (ExprApp (ExprVar "print") [ExprNum 2])
                        (ExprVar "u"))
            ~?= Right ValUnit

    , "Eval simple module" ~:
        evalModule (Module [
            Definition "main" [] (ExprVar "unit")
        ]) ~?= Right ValUnit
    ]

