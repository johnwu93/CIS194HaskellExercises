{-# LANGUAGE LambdaCase #-}

module HW1.HanoiSpec (main, spec) where

import           Control.Exception (evaluate)
import qualified Data.Map as Map
import           HW1.Hanoi             (Move, Peg, hanoi)
import           Test.Hspec
import           Data.List ()
import Control.Monad

type Pegs = Map.Map Peg [Int]

foldHelper ::  Maybe Pegs -> Move -> Maybe Pegs
foldHelper maybePegs move = maybePegs >>= (`processMove` move)




assertValidMoves :: Peg -> Peg -> Peg -> Int -> [Move] -> Bool
assertValidMoves sourcePeg destPeg tempPeg numDisks moves =
    let pegs = Map.fromList [(sourcePeg, [1..numDisks]), (destPeg, []), (tempPeg, [])] in
        case foldM processMove pegs moves of
            Nothing -> False
            Just _ -> True

checkList :: [Int] -> Maybe [Int]
checkList = \case
                [] -> Nothing
                diskList -> Just diskList


processMove :: Pegs -> Move -> Maybe Pegs
processMove pegs (source, dest) = do
    sourcePeg <- Map.lookup source pegs >>=  checkList
    destPeg <- Map.lookup dest pegs >>=  checkList
    let sourceDisk = head sourcePeg
    let destDisk = head destPeg
    if sourceDisk > destDisk
        then Nothing
        else Just (Map.insert dest (sourceDisk : destPeg) (Map.insert source (tail sourcePeg) pegs))



main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Hanoi" $ do
    it "should move 1 peg to another peg when the height is 1" $ do
        let actual = hanoi 1 "a" "b" "c"
        actual `shouldBe` [("a", "b")]


    it "should correctly match the moves " $
        hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]


