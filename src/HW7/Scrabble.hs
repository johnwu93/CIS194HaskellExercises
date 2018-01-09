{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module HW7.Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty = Score 0
    mappend (Score score1) (Score score2) = Score (score1 + score2)

score :: Char -> Score
score c = scoreHelper $ toLower c

scoreHelper :: Char -> Score
scoreHelper 'a' = Score 1
scoreHelper 'b' = Score 3
scoreHelper 'c' = Score 3
scoreHelper 'd' = Score 2
scoreHelper 'e' = Score 1
scoreHelper 'f' = Score 2
scoreHelper 'g' = Score 2
scoreHelper 'h' = Score 4
scoreHelper 'i' = Score 1
scoreHelper 'j' = Score 8
scoreHelper 'k' = Score 5
scoreHelper 'l' = Score 1
scoreHelper 'm' = Score 3
scoreHelper 'n' = Score 1
scoreHelper 'o' = Score 1
scoreHelper 'p' = Score 3
scoreHelper 'q' = Score 10
scoreHelper 'r' = Score 1
scoreHelper 's' = Score 1
scoreHelper 't' = Score 1
scoreHelper 'u' = Score 1
scoreHelper 'v' = Score 4
scoreHelper 'w' = Score 4
scoreHelper 'x' = Score 8
scoreHelper 'y' = Score 4
scoreHelper 'z' = Score 10
scoreHelper _ = Score 0

scoreString :: String -> Score
scoreString word = mconcat $ map scoreHelper word
