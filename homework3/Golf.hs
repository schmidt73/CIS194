skips :: [a] -> [[a]]
skips x = map (skip x) [1 .. (length x)]

skip :: [a] -> Int -> [a]
skip a n = case drop (n - 1) a of
        []     -> []
        [x]    -> [x]
        (x:xs) -> x : skip xs n

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b@(c:d:_))
    | (c > a) && (c > d) = c : (localMaxima b)
    | otherwise          = localMaxima b
localMaxima _ = []
