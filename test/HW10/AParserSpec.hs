module HW10.AParserSpec
  ( main
  , spec
  ) where

import HW10.AParser
       (Parser, abParser, abParser_, intOrUppercase, intPair, runParser)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, property)

main :: IO ()
main = hspec spec

instance Arbitrary a => Arbitrary (Parser a) where
  arbitrary = pure <$> arbitrary

spec :: Spec
spec =
  describe "HW10" $ do
    it "should have Parser abide to the applicable law" $
      property $ do
        parser <- arbitrary :: Gen (Parser Integer)
        string <- arbitrary :: Gen String
        f <- arbitrary :: Gen (Integer -> Integer)
        let leftResult = f `fmap` parser
        let rightResult = pure f <*> parser
        return $ runParser leftResult string == runParser rightResult string
    it
      "should have abParser parse the first two characters of a string if they are 'a' and 'b' and put them into a pair" $ do
      runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
      runParser abParser "aebcdf" `shouldBe` Nothing
    it "should have abParser_ parse the first two characters of a string if they are 'ab'" $ do
      runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
      runParser abParser_ "aebcdf" `shouldBe` Nothing
    it "should have intPair parse the first two digits of a string" $
      runParser intPair "12 34" `shouldBe` Just ([12, 34], "")
    it "should have intOrUppercase parse strings starting with ints or upper" $ do
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
      runParser intOrUppercase "foo" `shouldBe` Nothing
