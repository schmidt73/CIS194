{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.List

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [1 .. ]

fibs2 :: [Integer]
fibs2 = map fst . iterate (\(a, b) -> (b, a + b)) $ (0, 1)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a (interleaveStreams bs as)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
        fromInteger n = Cons n (streamRepeat 0)
        negate (Cons x xs) = Cons (-x) (negate xs)
        (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
        (*) (Cons x xs) b@(Cons y ys) = Cons (x * y) ((streamMap (*x) ys) + xs * b)

instance Show (Stream Integer) where
        show a = concat . map (\(a, b) -> (show a) ++ "x^" ++ (show b) ++ " ") . 
                 filter (\(a,b) -> if a == 0 then False else True) $ (zip xs [0 .. ])
                where xs = streamToList a

instance Fractional (Stream Integer) where
        (/) a@(Cons x xs) b@(Cons y ys) = 
