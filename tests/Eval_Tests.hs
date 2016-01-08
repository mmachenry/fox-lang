import Test.HUnit
import Parser
import Ast
import Eval
import Data.Ratio

main = runTestTT $ TestList [
    "A number" ~: evalExpr [] (ExprNum 413) `shouldBe` Right (ValNum 413)

    , "Bound vqriable" ~:
        evalExpr [("i", ValNum 413)] (ExprVar "i")
            `shouldBe` Right (ValNum 413)

    , "Apply a lambda to an argument" ~:
        evalExpr [] (ExprApp (ExprAbs [Parameter "x" TypeInferred]
                                      (ExprVar "x")) [ExprNum 413])
            `shouldBe` Right (ValNum 413)

    , "Apply curried lambda to two arguments" ~:
        evalExpr [] (ExprApp (ExprApp
                        (ExprAbs [Parameter "x" TypeInferred]
                                 (ExprAbs [Parameter "y" TypeInferred]
                                          (ExprApp (ExprVar "+") [ExprVar "x", ExprVar "y"])))
                        [ExprNum 413]) 
                        [ExprNum 0])
            `shouldBe` Right (ValNum 413)

    , "Simple if expression with true test." ~:
        evalExpr [] (ExprIfThenElse (ExprVar "true") (ExprNum 413) (ExprNum 2))
            `shouldBe` Right (ValNum 413)

    , "Simple if expression with false test." ~:
        evalExpr [] (ExprIfThenElse (ExprVar "false") (ExprNum 2) (ExprNum 413))
            `shouldBe` Right (ValNum 413)

    , "Add operator" ~:
        evalExpr [] (ExprApp (ExprVar "+") [ExprNum 412, ExprNum 1])
            `shouldBe` Right (ValNum 413)

    , "Sub operator" ~:
        evalExpr [] (ExprApp (ExprVar "-") [ExprNum 414, ExprNum 1])
            `shouldBe` Right (ValNum 413)

    , "Mul operator" ~:
        evalExpr [] (ExprApp (ExprVar "*") [ExprNum 7, ExprNum 59])
            `shouldBe` Right (ValNum 413)

    , "Div operator" ~:
        evalExpr [] (ExprApp (ExprVar "/") [ExprNum 826, ExprNum 2])
            `shouldBe` Right (ValNum 413)

    , "Noninteger division" ~:
        evalExpr [] (ExprApp (ExprVar "/") [ExprNum 44, ExprNum 14])
            `shouldBe` Right (ValNum (22%7))

    , "Equal operator" ~:
        evalExpr [] (ExprApp (ExprVar "==") [ExprNum 413, ExprNum 2])
            `shouldBe` Right (ValBool False)

    , "Not equal operator" ~:
        evalExpr [] (ExprApp (ExprVar "!=") [ExprNum 413, ExprNum 2])
            `shouldBe` Right (ValBool True)

    , "Gt operator" ~:
        evalExpr [] (ExprApp (ExprVar ">") [ExprNum 413, ExprNum 2])
            `shouldBe` Right (ValBool True)

    , "Lt operator" ~:
        evalExpr [] (ExprApp (ExprVar "<") [ExprNum 413, ExprNum 2])
            `shouldBe` Right (ValBool False)

    , "Gte operator" ~:
        evalExpr [] (ExprApp (ExprVar ">=") [ExprNum 413, ExprNum 413])
            `shouldBe` Right (ValBool True)

    , "Lte operator" ~:
        evalExpr [] (ExprApp (ExprVar "<=") [ExprNum 413, ExprNum 2])
            `shouldBe` Right (ValBool False)

    , "And operator" ~:
        evalExpr [] (ExprApp (ExprVar "&&") [ExprVar "true", ExprVar "false"])
            `shouldBe` Right (ValBool False)

    , "And operator" ~:
        evalExpr [] (ExprApp (ExprVar "||") [ExprVar "true", ExprVar "false"])
            `shouldBe` Right (ValBool True)

    , "Simple run, allocate, write, read block." ~:
        evalExpr [] (ExprRun
                (ExprEffectBind "x" (ExprApp (ExprVar "newref") [ExprNum 2])
                    (ExprCompound
                        (ExprApp (ExprVar ":=") [ExprVar "x", ExprNum 413])
                        (ExprApp (ExprVar "!") [ExprVar "x"])))
            ) `shouldBe` Right (ValNum 413)

    , "Let bind expression { x = 413; x} " ~:
        evalExpr [] (ExprLetBind "x" (ExprNum 413) (ExprVar "x"))
            `shouldBe` Right (ValNum 413)

    , "Compound expression { ignore(2); 413 }" ~:
        evalExpr [] (ExprCompound (ExprApp (ExprVar "ignore") [ExprNum 2])
                                  (ExprNum 413))
            `shouldBe` Right (ValNum 413)

    , "Effect bind expression { u <- ignore(2); u }" ~:
        evalExpr [] (ExprEffectBind "u" (ExprApp (ExprVar "ignore") [ExprNum 2])
                        (ExprVar "u"))
            `shouldBe` Right ValUnit

    , "Eval simple module" ~: TestCase $ do
        result <- evalModule (Module [Definition "main" [] (ExprVar "unit")])
        assertEqual "" result (Right ValUnit)

    , "Repeat { x <- newref(206); repeat (207) { x := !x + 1 } !x" ~:
        evalExpr [] (ExprRun (ExprEffectBind "x" (ExprApp (ExprVar "newref") [ExprNum 206])
                        (ExprCompound
                            (ExprRepeat (ExprNum 207)
                                (ExprApp (ExprVar ":=") [
                                    ExprVar "x",
                                    (ExprApp (ExprVar "+") [ExprApp (ExprVar "!") [ExprVar "x"], ExprNum 1]) ] ))
                            (ExprApp (ExprVar "!") [ExprVar "x"]))))
            `shouldBe` Right (ValNum 413)
    ]

-- A simple helper function for encapsulating running the evaluator in IO and creating a
-- test case suitable for HUnit.
shouldBe e v = TestCase $ do
    result <- runEval e
    assertEqual "" v result

