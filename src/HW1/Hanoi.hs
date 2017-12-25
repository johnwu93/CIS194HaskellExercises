module HW1.Hanoi (hanoi, Peg, Move) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 sourcePeg destPeg tempPeg = [(sourcePeg, destPeg)]
hanoi height sourcePeg destPeg tempPeg =
    fromSourceToTempMoves ++ baseToDestMove ++ fromTempToDestMove
    where fromSourceToTempMoves = hanoi (height - 1) sourcePeg tempPeg destPeg
          baseToDestMove = hanoi 1 sourcePeg destPeg tempPeg
          fromTempToDestMove = hanoi (height - 1) tempPeg destPeg sourcePeg
