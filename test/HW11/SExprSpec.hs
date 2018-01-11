{-# OPTIONS_GHC -Wall #-}

module HW11.SExprSpec
  ( main
  , spec
  ) where

import Data.Char (isUpper)
import HW10.AParser (runParser, satisfy)
import HW11.SExpr
       (excludeRule, ident, number, oneOrMore, parseSExpr, spaces,
        zeroOrMore, SExpr(..))
import Test.Hspec
       (Spec, describe, hspec, it, shouldBe, shouldNotBe)
import Data.Maybe (isNothing,isJust)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "HW11" $ do
    it "should parse multiple times with zeroOrMore" $ do
      runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
      runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("", "abcdeFGh")
    it "should parse multiple times with oneOrMore" $ do
      runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
      runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing
    it "should correctly parse an identifier" $ do
      runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
      runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
      runParser ident "2bad" `shouldBe` Nothing
      runParser ident "" `shouldBe` Nothing
    it "should correctly parse a number" $ do
      runParser number "123 foo" `shouldBe` Just (123, " foo")
      runParser number "foo" `shouldBe` Nothing
      runParser number "123foo" `shouldBe` Nothing
    describe "should correctly parse a student of atoms" $ do
      it "123 foo" $
        isJust (runParser parseSExpr "123 foo")
      it "12 a" $
        isJust (runParser parseSExpr "12 a")
      it ":)" $
        isNothing (runParser parseSExpr ":)")
      it "(null char)" $
        isJust (runParser parseSExpr "")
      it "12 :)" $
        isNothing (runParser parseSExpr "12 :)")
