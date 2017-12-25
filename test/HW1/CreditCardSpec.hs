module HW1.CreditCardSpec (main, spec) where

import           Control.Exception (evaluate)
import           HW1.CreditCard        (toDigits)
import           Test.Hspec        (anyException, describe, hspec, it, shouldBe,
                                    shouldThrow, Spec)


main :: IO ()
main = hspec spec


spec :: Spec
spec =
  describe "Credit Card Number" $
    it "should get find the digits of a number" $
      toDigits 1234 `shouldBe` [1, 2, 3, 4]
