module HW6.FibonacciSpec (main, spec) where

import Test.Hspec

import Test.QuickCheck

import HW6.Fibonacci


main :: IO ()
main = hspec spec

showFirst20 :: Show a => [a] -> String
showFirst20 = show . take 20

testArbitaryAmount :: (Show a, Eq a) => Int -> Stream a -> [a] -> IO()
testArbitaryAmount n stream infList = take n (streamToList stream) `shouldBe` take n infList

instance Arbitrary a => Arbitrary (Stream a) where
    arbitrary = Cons <$> arbitrary <*> arbitrary

testStream :: Eq a => Integer -> Stream a -> [a]-> Bool
testStream n stream infList = take 100 (streamToList stream) == take 100 infList


class ComparableInfList l where
    test :: Eq a => Int -> l a -> [a] -> Bool

instance ComparableInfList Stream where
    test n stream infList = take n (streamToList stream) == take n infList

instance ComparableInfList [] where
    test n list infList = take n list == take n infList

(<==>) :: (ComparableInfList l, Eq a)=> l a -> [a] -> Bool
(<==>) = test 100

toPolynomial :: [Integer] -> [Integer]
toPolynomial terms = terms ++ [0, 0..]

spec :: Spec
spec = describe "HW6" $ do

    it "should compute fib correctly" $
        test 15 fibs1 [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

    it "should compute fib2 correctly" $
        test 30 fibs1 fibs2

    it "should have streams repeat a value indefinetly" $
        streamRepeat 10 <==> repeat 10

    it "should map a value indefinetly" $
        streamRepeat 10 <==> repeat 10

    it "should map a set of infinite values" $ property (\stream -> do
            fun <- arbitrary :: Gen(Int -> Int)
            return $ streamMap fun stream <==> map fun ( streamToList stream)
        )

    it "should have streamFromSeed be equivalent to iterate for Lists" $ property (\startingPoint -> do
           fun <- arbitrary :: Gen(Int -> Int)
           return $ streamFromSeed fun startingPoint <==> iterate fun startingPoint
       )

    it "should produce all natural numbers" $
        streamMap Prelude.fromInteger nats <==> [0, 1..]

    it "should produce all a list of numbers such where the nth element is the largest power of 2 which evenly divides n." $
        test 16 ruler [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]

    describe "generating functions" $ do
        it "should compute x^4" $ (x ^ 4 :: Stream Integer) <==> toPolynomial [0, 0, 0, 0, 1]

        it "should compute (x + 1) ^ 5" $ ((1 + x) ^ 5 :: Stream Integer) <==> toPolynomial [1, 5, 10, 10, 5, 1]

        it "should compute (x^2 + x + 3) * (x - 5)" $
            ((3 + x + x^2) * (x - 5) :: Stream Integer) <==> toPolynomial [-15, -2, -4, 1]

        it "should compute x / (1 - x - x^2) as a Fibonacci sequence" $
            fibs3 <==> fibs2

    it "should have fib4 compute fibonacci" $ property (\(Positive n) ->
            fib4 n == fibs2 !! fromInteger n
        )



