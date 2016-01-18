import Test.HUnit
import Parser
import Ast
import Eval
import Data.Ratio
import qualified Data.Map as Map
import Control.Monad.Reader

main = runTestTT $ TestList [
    "A number" ~: evalExpr (ExprLiteral $ ValNum 413) `shouldBe` Right (ValNum 413)

    , "Bound vqriable" ~:
        local (const (Map.fromList [("i", ValNum 413)])) (evalExpr (ExprVar "i"))
            `shouldBe` Right (ValNum 413)

    , "Apply a lambda to an argument" ~:
        evalExpr (ExprApp (ExprAbs [Parameter "x" TypeInferred]
                                      (ExprVar "x")) [ExprLiteral $ ValNum 413])
            `shouldBe` Right (ValNum 413)

    , "Apply curried lambda to two arguments" ~:
        evalExpr (ExprApp (ExprApp
                        (ExprAbs [Parameter "x" TypeInferred]
                                 (ExprAbs [Parameter "y" TypeInferred]
                                          (ExprApp (ExprVar "+") [ExprVar "x", ExprVar "y"])))
                        [ExprLiteral $ ValNum 413]) 
                        [ExprLiteral $ ValNum 0])
            `shouldBe` Right (ValNum 413)

    , "Simple if expression with true test." ~:
        evalExpr (ExprIfThenElse (ExprVar "true") (ExprLiteral $ ValNum 413) (ExprLiteral $ ValNum 2))
            `shouldBe` Right (ValNum 413)

    , "Simple if expression with false test." ~:
        evalExpr (ExprIfThenElse (ExprVar "false") (ExprLiteral $ ValNum 2) (ExprLiteral $ ValNum 413))
            `shouldBe` Right (ValNum 413)

    , "Add operator" ~:
        evalExpr (ExprApp (ExprVar "+") [ExprLiteral $ ValNum 412, ExprLiteral $ ValNum 1])
            `shouldBe` Right (ValNum 413)

    , "Sub operator" ~:
        evalExpr (ExprApp (ExprVar "-") [ExprLiteral $ ValNum 414, ExprLiteral $ ValNum 1])
            `shouldBe` Right (ValNum 413)

    , "Mul operator" ~:
        evalExpr (ExprApp (ExprVar "*") [ExprLiteral $ ValNum 7, ExprLiteral $ ValNum 59])
            `shouldBe` Right (ValNum 413)

    , "Div operator" ~:
        evalExpr (ExprApp (ExprVar "/") [ExprLiteral $ ValNum 826, ExprLiteral $ ValNum 2])
            `shouldBe` Right (ValNum 413)

    , "Noninteger division" ~:
        evalExpr (ExprApp (ExprVar "/") [ExprLiteral $ ValNum 44, ExprLiteral $ ValNum 14])
            `shouldBe` Right (ValNum (22%7))

    , "Equal operator" ~:
        evalExpr (ExprApp (ExprVar "==") [ExprLiteral $ ValNum 413, ExprLiteral $ ValNum 2])
            `shouldBe` Right (ValBool False)

    , "Not equal operator" ~:
        evalExpr (ExprApp (ExprVar "!=") [ExprLiteral $ ValNum 413, ExprLiteral $ ValNum 2])
            `shouldBe` Right (ValBool True)

    , "Gt operator" ~:
        evalExpr (ExprApp (ExprVar ">") [ExprLiteral $ ValNum 413, ExprLiteral $ ValNum 2])
            `shouldBe` Right (ValBool True)

    , "Lt operator" ~:
        evalExpr (ExprApp (ExprVar "<") [ExprLiteral $ ValNum 413, ExprLiteral $ ValNum 2])
            `shouldBe` Right (ValBool False)

    , "Gte operator" ~:
        evalExpr (ExprApp (ExprVar ">=") [ExprLiteral $ ValNum 413, ExprLiteral $ ValNum 413])
            `shouldBe` Right (ValBool True)

    , "Lte operator" ~:
        evalExpr (ExprApp (ExprVar "<=") [ExprLiteral $ ValNum 413, ExprLiteral $ ValNum 2])
            `shouldBe` Right (ValBool False)

    , "And operator" ~:
        evalExpr (ExprApp (ExprVar "&&") [ExprVar "true", ExprVar "false"])
            `shouldBe` Right (ValBool False)

    , "And operator" ~:
        evalExpr (ExprApp (ExprVar "||") [ExprVar "true", ExprVar "false"])
            `shouldBe` Right (ValBool True)

    , "Simple run, allocate, write, read block." ~:
        evalExpr (ExprRun
                (ExprEffectBind "x" (ExprApp (ExprVar "newref") [ExprLiteral $ ValNum 2])
                    (ExprCompound
                        (ExprApp (ExprVar ":=") [ExprVar "x", ExprLiteral $ ValNum 413])
                        (ExprApp (ExprVar "!") [ExprVar "x"])))
            ) `shouldBe` Right (ValNum 413)

    , "Let bind expression { x = 413; x} " ~:
        evalExpr (ExprLetBind "x" (ExprLiteral $ ValNum 413) (ExprVar "x"))
            `shouldBe` Right (ValNum 413)

    , "Compound expression { ignore(2); 413 }" ~:
        evalExpr (ExprCompound (ExprApp (ExprVar "ignore") [ExprLiteral $ ValNum 2])
                                  (ExprLiteral $ ValNum 413))
            `shouldBe` Right (ValNum 413)

    , "Effect bind expression { u <- ignore(2); u }" ~:
        evalExpr (ExprEffectBind "u" (ExprApp (ExprVar "ignore") [ExprLiteral $ ValNum 2])
                        (ExprVar "u"))
            `shouldBe` Right ValUnit

    , "Eval simple module" ~: TestCase $ do
        result <- evalModule (Module [Definition "main" [] (ExprVar "unit")])
        assertEqual "" result (Right ValUnit)

    , "Repeat { x <- newref(206); repeat (207) { x := !x + 1 } !x" ~:
        evalExpr (ExprRun (ExprEffectBind "x" (ExprApp (ExprVar "newref") [ExprLiteral $ ValNum 206])
                        (ExprCompound
                            (ExprRepeat (ExprLiteral $ ValNum 207)
                                (ExprApp (ExprVar ":=") [
                                    ExprVar "x",
                                    (ExprApp (ExprVar "+") [ExprApp (ExprVar "!") [ExprVar "x"], ExprLiteral $ ValNum 1]) ] ))
                            (ExprApp (ExprVar "!") [ExprVar "x"]))))
            `shouldBe` Right (ValNum 413)
    ]

-- A simple helper function for encapsulating running the evaluator in IO and creating a
-- test case suitable for HUnit.
shouldBe e v = TestCase $ do
    result <- runEval e
    assertEqual "" v result

