{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW12.Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values
newtype DieValue = DV
  { unDV :: Int
  } deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk
type Army = Int

data Battlefield = Battlefield
  { attackers :: Army
  , defenders :: Army
  } deriving Show

computeLoss :: [Int] -> [Int] -> (Int, Int)
computeLoss attackerDice defenderDice =
  let attackResults = map attackEffect $ zipWith (>) attackerDice defenderDice
  in foldl (\(accX, accY) (x, y) -> (accX + x, accY + y)) (0, 0) attackResults
  where
    attackEffect True = (0, 1)
    attackEffect False = (1, 0)


battle :: Battlefield -> Rand StdGen Battlefield
battle battleField@(Battlefield totalAttackers totalDefenders) = do
  let numAttackersToBattle = min 3 (totalAttackers - 1)
  let numDefendersToBattle = min 2 totalDefenders
  attackerRolls <- simulateRolls numAttackersToBattle
  defenderRolls <- simulateRolls numDefendersToBattle
  let (attackerLoss, defenderLoss) = computeLoss (map unDV attackerRolls) (map unDV defenderRolls)
  return $ Battlefield (totalAttackers - attackerLoss) (totalDefenders - defenderLoss)
  where simulateRolls = flip replicateM die

invade :: Battlefield -> Rand StdGen Battlefield
invade battleField@(Battlefield 0 _) = return battleField
invade battleField@(Battlefield 1 _) = return battleField
invade battleField@(Battlefield _ 0) = return battleField
invade battleField = battle battleField >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb battleField = do
  let numSimulations = 1000
  simulations <- replicateM numSimulations (invade battleField)
  let numWins = length $ filter isWin simulations
  return $ fromIntegral numWins / fromInteger (toInteger numSimulations)
  where
    isWin (Battlefield attackers defenders) = attackers > defenders
