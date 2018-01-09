{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module HW7.JoinList where

import Data.Monoid
import HW7.Sized
import HW7.Scrabble
import HW7.Buffer

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ m = m
m +++ Empty = m
left +++ right = Append (tag left <> tag right) left right

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

toJoinList :: Monoid m => [(m, a)] -> JoinList m a
toJoinList list = foldl (+++) Empty leaves
                  where leaves = map (uncurry Single) list

computeSize :: (Sized b, Monoid b) => JoinList b a -> Int
computeSize (Single annotation _ ) = getSize (size annotation)
computeSize (Append annotation _ _) = getSize (size annotation)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ value) = Just value
indexJ _ (Single _ _) = Nothing
indexJ i joinList@(Append _ left right)
    | i < leftSize = indexJ i left
    | otherwise = indexJ (i - leftSize) right
    where leftSize = computeSize left

replaceJ :: (Sized b, Monoid b) => (a -> b) -> Int -> a -> JoinList b a -> JoinList b a
replaceJ _ _ _ Empty = Empty
replaceJ transform 0 value single@(Single _ _) = Single (transform value) value
replaceJ transform i value joinList@(Append _ left right)
    | i < leftSize = replaceJ transform i value left
    | otherwise = replaceJ transform (i - leftSize) value right
    where leftSize = computeSize left


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 joinList = joinList
dropJ n Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n joinList@(Append _ left right)
    | n < leftSize = dropJ n left +++ right
    | otherwise = dropJ (n - leftSize)  right
    where leftSize = computeSize left


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 joinList = joinList
takeJ n Empty = Empty
takeJ n single@(Single _ _) = single
takeJ n joinList@(Append _ left right)
    | n <= leftSize = takeJ n left
    | otherwise = left +++ takeJ (n - leftSize) right
    where leftSize = computeSize left


(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_  !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)


scoreLine :: String -> JoinList Score String
scoreLine word = Single (scoreString word) word

lineAttribute :: String -> (Score, Size)
lineAttribute line = (scoreString line, Size 1)

instance Buffer (JoinList (Score, Size) String) where
    toString = mconcat . jlToList
    fromString text = let words = lines text in
                        toJoinList $ zip (map lineAttribute words) words
    replaceLine = replaceJ lineAttribute
    line = indexJ
    numLines = getSize . snd . tag
    value = getScore . fst . tag
