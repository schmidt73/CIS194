{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import Parser
import ExprT
import StackVM

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (ExprT.Mul x y) = (eval x) * (eval y)
eval (ExprT.Add x y) = (eval x) + (eval y)

evalStr :: String -> Maybe Integer
evalStr s = case e of
        Nothing     -> Nothing
        (Just expr) -> Just (eval expr)
   where e = parseExp Lit ExprT.Add ExprT.Mul s

class Expr a where
        lit :: Integer -> a
        add :: a -> a -> a
        mul :: a -> a -> a

instance Expr ExprT where
        lit x = Lit x
        add x y = ExprT.Add x y 
        mul x y = ExprT.Mul x y

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
        lit x = x
        mul x y = x * y
        add x y = x + y

instance Expr Bool where
        lit x = if x > 0 then True else False
        mul x y = x && y
        add x y = x || y

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
        lit x = MinMax x
        mul (MinMax x) (MinMax y) = lit (min x y)
        add (MinMax x) (MinMax y) = lit (max x y)

instance Expr Mod7 where
        lit x = Mod7 (mod x 7)
        mul (Mod7 x) (Mod7 y) = lit (x * y)
        add (Mod7 x) (Mod7 y) = lit (x + y)

instance Expr Program where
        lit x = [PushI x]
        mul x y = x ++ y ++ [StackVM.Mul]
        add x y = x ++ y ++ [StackVM.Add]

compile :: String -> Maybe Program
compile s = case e of
        Nothing     -> Nothing
        (Just expr) -> Just expr
   where e = parseExp lit add mul s :: Maybe Program
