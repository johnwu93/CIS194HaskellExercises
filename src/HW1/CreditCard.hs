module HW1.CreditCard
(toDigits
)where

toDigits :: Integer -> [Integer]
toDigits n
   | n <= 0      = []
   | n < 10     = [n]
   | otherwise  = toDigits ( div n  10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

data SubResult = SubResult {isOdd :: Bool,  list :: [Integer]}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = let SubResult _ result = helper xs
                      in result



helper :: [Integer] -> SubResult
helper xs =
    case xs of [] -> SubResult False []
               [x] -> SubResult True [x]
               x1:x2:xs -> let SubResult isOdd subList = helper xs
                           in  (
                                if isOdd then SubResult True (x1 : 2 * x2 : subList)
                                else SubResult False (2* x1 : x2 : subList)
                                )

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (sum . toDigits) xs)


validate :: Integer -> Bool
validate cardNumber = let summedValue = sumDigits . doubleEveryOther $ toDigits cardNumber
                      in mod summedValue 10 == 0

