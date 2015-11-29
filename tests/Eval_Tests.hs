import Test.HUnit
import Parser
import Ast
import Eval

main = runTestTT $ TestList [
    "A number" ~:
        evalExpr [] (ExprNum 413) ~?= Right (ValNum 413)

    , "Bound vqriable" ~:
        evalExpr [("i", ValNum 413)] (ExprVar "i") ~?= Right (ValNum 413)

    , "Apply a lambda to an argument" ~:
        evalExpr [] (ExprApp (ExprAbs [Parameter "x" TypeInferred] (ExprVar "x")) [ExprNum 413])
            ~?= Right (ValNum 413)

    , "Simple if expression with true test." ~:
        evalExpr [] (ExprIfThenElse (ExprBool True) (ExprNum 413) (ExprNum 2))
            ~?= Right (ValNum 413)

    , "Simple if expression with false test." ~:
        evalExpr [] (ExprIfThenElse (ExprBool False) (ExprNum 2) (ExprNum 413))
            ~?= Right (ValNum 413)

    , "Add operator" ~:
        evalExpr [] (ExprBinOp Add (ExprNum 412) (ExprNum 1)) ~?= Right (ValNum 413)

    , "Sub operator" ~:
        evalExpr [] (ExprBinOp Sub (ExprNum 414) (ExprNum 1)) ~?= Right (ValNum 413)

    , "Mul operator" ~:
        evalExpr [] (ExprBinOp Mul (ExprNum 7) (ExprNum 59)) ~?= Right (ValNum 413)

    , "Div operator" ~:
        evalExpr [] (ExprBinOp Div (ExprNum 826) (ExprNum 2)) ~?= Right (ValNum 413)

    , "Equal operator" ~:
        evalExpr [] (ExprBinOp BoolEq (ExprNum 413) (ExprNum 2)) ~?= Right (ValBool False)

    , "Not equal operator" ~:
        evalExpr [] (ExprBinOp Ne (ExprNum 413) (ExprNum 2)) ~?= Right (ValBool True)

    , "Gt operator" ~:
        evalExpr [] (ExprBinOp Gt (ExprNum 413) (ExprNum 2)) ~?= Right (ValBool True)

    , "Lt operator" ~:
        evalExpr [] (ExprBinOp Lt (ExprNum 413) (ExprNum 2)) ~?= Right (ValBool False)

    , "Gte operator" ~:
        evalExpr [] (ExprBinOp Gte (ExprNum 413) (ExprNum 413)) ~?= Right (ValBool True)

    , "Lte operator" ~:
        evalExpr [] (ExprBinOp Lte (ExprNum 413) (ExprNum 2)) ~?= Right (ValBool False)

    , "And operator" ~:
        evalExpr [] (ExprBinOp And (ExprBool True) (ExprBool False)) ~?= Right (ValBool False)

    , "And operator" ~:
        evalExpr [] (ExprBinOp Or (ExprBool True) (ExprBool False)) ~?= Right (ValBool True)

    -- FIXME: Implement after decision of how to parse binding constructs
    , "Simple run, allocate, write, read block." ~:
        evalExpr [] (ExprRun [
                ExprEffectBind "x" (ExprAlloc (ExprNum 2)),
                ExprWrite (ExprVar "x") (ExprAlloc (ExprNum 413)),
                ExprRead (ExprVar "x")
            ]) ~?= Right (ValNum 413)
    ]

