import Data.Set (Set)
import qualified Data.Set as Set

-- Problem 1.

fun1 :: [Integer] -> Integer
fun1 = product . map ((+) (-2)) . filter ((==0) . (`mod` 2)) 

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . 
        iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Problem 2.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insertNode :: a -> Tree a -> Tree a
insertNode newNode Leaf = Node 0 Leaf newNode Leaf
insertNode newNode (Node _ oldLeft node oldRight) =
  let (newLeft, newRight) =
        if height oldLeft < height oldRight
        then (insertNode newNode oldLeft, oldRight)
        else (oldLeft, insertNode newNode oldRight) in
   let newHeight = 1 + max (height newLeft) (height newRight) in
    Node newHeight newLeft node newRight

height :: Tree a -> Integer
height Leaf = (-1)
height (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

-- Problem 3.
xor :: [Bool] -> Bool
xor = foldr (\a b -> if a then not b else b) False

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\a b -> (f a) : b) [] xs

-- Problem 4

-- One way:
-- [x | i <- [1 .. n], j <- [1 .. n], let x = i + j + 2*i*j, x < n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2) . (\\) [1 .. n] 
        $ [x | i <- [1 .. n], j <- [1 .. n], let x = i + j + 2*i*j, x < n]

