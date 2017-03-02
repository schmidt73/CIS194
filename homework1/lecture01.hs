factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial ((-) n 1) 

isOdd :: Int -> Bool
isOdd n 
     | n `mod` 2 == 1 = True
     | otherwise      = False
