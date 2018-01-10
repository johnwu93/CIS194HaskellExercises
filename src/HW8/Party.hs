module HW8.Party where

import Data.Tree
import Data.List (sort)
import Data.Traversable (forM)
import HW8.Employee

glCons :: Employee -> GuestList -> GuestList
glCons newEmployee@(Emp _ newFun) (GL employees totalFun) = GL (newEmployee : employees) (totalFun + newFun)

instance Monoid GuestList where
  mempty = GL [] 0
  (GL leftEmployees leftFun) `mappend` (GL rightEmployees rightFun) =
    GL (leftEmployees ++ rightEmployees) (leftFun + rightFun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun leftGuestList@(GL _ leftFun) rightGuestList@(GL _ rightFun)
  | leftFun >= rightFun = leftGuestList
  | otherwise = rightGuestList

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node rootLabel subForest) = f rootLabel (treeFold f <$> subForest)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (glCons boss mempty, mempty)
nextLevel boss guessListPair =
  let (_, excludedSubordinatesGL) = unzip guessListPair
      excludedBossGL = mconcat (map (uncurry moreFun) guessListPair)
      invitedBossGL = glCons boss $ mconcat excludedSubordinatesGL
  in (invitedBossGL, excludedBossGL)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


main :: IO()
main = do
  fileContents <- readFile "src/HW8/company.txt"
  let employees = read fileContents
  let (GL employeeList fun)= maxFun (employees :: Tree Employee)
  putStrLn $ "Total fun:" ++ show fun
  mapM_ print (sort $ map empName employeeList)
