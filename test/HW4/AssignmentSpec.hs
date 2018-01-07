module HW4.AssignmentSpec  (main, spec) where

import Test.QuickCheck

import HW4.Assignment

import Data.Numbers.Primes (primes)

import Test.Hspec
  ( Spec
  , anyException
  , describe
  , hspec
  , it
  , shouldBe
  , shouldThrow
  )


main :: IO ()
main = hspec spec


prop_func2 (Positive value) = fun2 value == fun2' value

spec :: Spec
spec =
    describe "HW 4" $ do
        it "should have fun1' equal to fun1'" $ property (\list -> fun1 list == fun1' list)

        it "should have fun2' equal to fun2'" $ property (\(Positive value) -> fun2 value == fun2' value)

        describe "More Folds!" $ do
            it "should have xor be True for odd number of True, otherwise False" $ property
                (\boolList -> ((== 1) $ (`mod` 2) $ sum $ map (\x -> if x then 1 else 0) boolList) == xor boolList)

            it "should have map' be equivalent to map" $
                let values = [1, 2, 3] in map (+ 3) values `shouldBe` map' (+ 3) values

            it "should have myfoldl be equivalent to foldl" $
                let values = [1, 2, 3] in myFoldl (+) 0 values `shouldBe` sum values

            it "should have sieveSundaram produce a list of primes until 2 * n + 2" $ property
                (\(Positive n) -> takeWhile (< 2 * n + 2) primes == 2:sieveSundaram n)
