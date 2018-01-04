module Golf where

skips :: [a] -> [[a]]
skips list = [getEveryOther list index | index <- [1 .. length list]]



getEveryOther :: [a] -> Int -> [a]
getEveryOther [] _ = []
getEveryOther xs index = case drop (index - 1) xs of
    [] -> []
    y:yTail -> y:getEveryOther yTail index

exercise1 = do
  print $ skips "ABCD"
  print $ skips "hello!"
  print $ skips [1]
  print $ skips [True,False]
  print True


localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (left:xs@(mid:right:_))
    | left < mid && right < mid = mid:localMaxima xs
    | otherwise = localMaxima xs



main = exercise1
