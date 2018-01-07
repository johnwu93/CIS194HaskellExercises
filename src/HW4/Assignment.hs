module HW4.Assignment (fun1, fun1', fun2, fun2', foldTree, xor, map', myFoldl, sieveSundaram) where
import Data.List ((\\))
{-# ANN module ("HLint: ignore Use map"::String) #-}

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ filter even $ takeWhile (/= 1) $ iterate (\k -> if even k then k `div` 2 else 3 * k + 1) n


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)
foldTree :: [a] -> Tree a
foldTree = foldl insert Leaf


computeHeight :: Tree a -> Tree a -> Integer
computeHeight Leaf Leaf = 1
computeHeight Leaf (Node rightHeight _ _ _) = rightHeight
computeHeight (Node leftHeight _ _ _) Leaf = leftHeight
computeHeight (Node leftHeight _ _ _) (Node rightHeight _ _ _) = max leftHeight rightHeight + 1


insert :: Tree a -> a -> Tree a
insert Leaf input = Node 0 Leaf input Leaf
insert (Node 0 Leaf value Leaf) input = Node 1 (insert Leaf input) value Leaf
insert (Node level Leaf value right) input = Node level (insert Leaf input) value right
insert (Node level left value Leaf) input = Node level  left value (insert Leaf input)
insert (Node level left@(Node leftHeight _ _ _) value right@(Node rightHeight _ _ _)) input
    | leftHeight >= rightHeight = let newRight = insert right input in Node (computeHeight left newRight) left value newRight
    | otherwise = let newLeft = insert left input in Node (computeHeight newLeft right) newLeft value right


xor :: [Bool] -> Bool
xor = foldl (\acc newValue -> if newValue then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\input accValues -> f input:accValues) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\value gacc acc -> gacc (f acc value)) id xs base

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\primeCanidate -> 2 * primeCanidate + 1) $ [1 .. n] \\ [i + j + 2 * i * j | i <- [1..((n-1) `div` 3)], j <- [i..((n-i) `div` 2*i + 1)]]

main = show $ sieveSundaram 10
