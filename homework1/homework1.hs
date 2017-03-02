-- Excersises #1-4

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
      | n < 10    = [n]
      | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x 
      | x == []                 = []
      | (length x) `mod` 2 == 0 = ((head x) * 2) : doubleEveryOther (tail x)
      | otherwise               = (head x) : doubleEveryOther (tail x)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) 
      | x < 10    = x + sumDigits xs
      | otherwise = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Towers of Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _   _    _   = []
hanoi n src dest aux = hanoi (n - 1) src aux dest ++ [(src, dest)] ++ hanoi (n - 1) dest aux src

