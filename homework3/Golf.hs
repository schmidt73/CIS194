skips :: [a] -> [[a]]
skips x = map (skip x) [1 .. (length x)]

skip :: [a] -> Int -> [a]
skip a n = case drop (n - 1) a of
        []     -> []
        [x]    -> [x]
        (x:xs) -> x : skip xs n

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b@(c:d:_))
    | c > a && c > d = c : localMaxima b
    | otherwise          = localMaxima b
localMaxima _ = []

histogram :: [Int] -> String
histogram l = cHist (count l) "==========\n0123456789"

cHist :: [Int] -> String -> String
cHist (0:0:0:0:0:0:0:0:0:0:[]) s = s
cHist l s = cHist (map (\x -> if x > 0 then x - 1 else 0) l) 
                  ((map (\x -> if x > 0 then '*' else ' ') l) ++ "\n" ++ s)

count :: [Int] -> [Int]
count = hc (replicate 10 0)

hc :: [Int] -> [Int] -> [Int]
hc l (x:xs)  = let (a, b) = splitAt x l
               in hc (a ++ [(head b) + 1] ++ tail b) xs
hc l []      = l
