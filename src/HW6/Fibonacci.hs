{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module HW6.Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)


data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons val tail) = val:streamToList tail

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList


streamRepeat :: a -> Stream a
streamRepeat val = Cons val $ streamRepeat val

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons val stream) = Cons (f val) (streamMap f stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f val = Cons val (streamFromSeed f (f val))

nats :: Stream Integer
nats = streamFromSeed succ 0

computeLargestEvenlyDivided2 :: Integer -> Integer
computeLargestEvenlyDivided2 = toInteger . length . takeWhile (\value -> value > 0 && value `mod` 2 == 0) . iterate (`div` 2)



-- TODO 鞋跟好
ruler :: Stream Integer
ruler = streamMap (computeLargestEvenlyDivided2 . (+ 1)) nats

type GeneratorFunction = Stream Integer

x :: GeneratorFunction
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num GeneratorFunction where
    fromInteger n = Cons n $ streamRepeat 0
    negate = streamMap (* (-1))
    (Cons headX tailX) + (Cons headY tailY) = Cons (headX + headY) (tailX + tailY)
    (Cons headX tailX) * y@(Cons headY tailY) = Cons (headX * headY) (streamMap (* headX) tailY + tailX * y)

times :: Integer -> GeneratorFunction -> GeneratorFunction
constant `times` generatorFunction = streamMap (* constant) generatorFunction

instance Fractional GeneratorFunction where
    x@(Cons headX tailX) / y@(Cons headY tailY) = Cons head tail
        where head = headX `div` headY
              tail = (1 `div` headY) `times` (tailX - (x / y) * tailY)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

type Matrix = ((Integer, Integer), (Integer, Integer))

instance Num Matrix where
    ((x11, x21), (x12, x22)) * ((y11, y21), (y12, y22)) = ((x11 * y11 + x21 * y12, x11 * y21 + x21 * y22),
                                                           (x12 * y11 + x22 * y12, x12 * x21 + x22 * y22))

fib4 :: Integer -> Integer
fib4 n = fst $ snd resultMatrix where
    startingMatrix =  ((1, 1), (1, 0)) :: Matrix
    resultMatrix = startingMatrix ^ n


