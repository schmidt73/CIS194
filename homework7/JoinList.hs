import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append ((tag l) <> (tag r)) l r

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i - 1)

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize (Single _ _) = 1
jlSize jl           = getSize . size . tag $ jl

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append m l r)
        | i < k     = indexJ i l
        | otherwise = indexJ (i - k) r
   where k = jlSize l

