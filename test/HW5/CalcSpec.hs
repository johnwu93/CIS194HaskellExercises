module HW5.CalcSpec (main, spec) where
{-# LANGUAGE TypeOperators #-}

import Test.Hspec (hspec, Spec, describe, it, shouldBe)
import qualified HW5.ExprT as ExprT
import HW5.Calc
import HW5.Parser
import Data.Maybe



main :: IO ()
main = hspec spec


testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

spec :: Spec
spec = describe "HW5" $ do
    it "Should evaluate an expression tree correctly" $
        eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) == 20

    it "should parse an expression correctly" $ fromJust (evalStr "(2+3)*4") `shouldBe` 20

    it "should generalize ExprT with Expr" $
        (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT.ExprT) `shouldBe` ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)

    describe "Expr instances" $ do
        it "should calculate integers normally when Expr is derived from integers" $
            fromJust (testExp  "(3 * -4) + 5") `shouldBe` (-7 :: Integer)

        it "should calculate boolean operations when Expr is derived from bool" $
            fromJust (testExp  "(3 * -4) + 5") `shouldBe` True

    describe "map functional instances" $ do
        it "should compute a valid expression if all variables are known" $
            (withVars [("x", 6)] $ add (lit 3) (var "x")) `shouldBe` Just 9

        it "should not compute expression if a variable is unknown" $
            (withVars [("x", 6)] $ add (lit 3) (var "y")) `shouldBe` Nothing

        it "should not compute expression if a variable is unknown" $
            (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))) `shouldBe` Just 54


