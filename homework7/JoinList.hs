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

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                                    = Nothing
indexJ i (Append m _ _) | i >= (getSize (size m)) = Nothing

indexJ 0 (Single _ a) = Just a
indexJ i (Append m l@(Append lm _ _) r)
        | i >= ls   = indexJ (i - ls) r
        | otherwise = indexJ i l
   where s = getSize . size $ m
         ls = getSize . size $ lm

indexJ i (Append m l r@(Append rm _ _)) = indexJ (i - (s - rs)) r
   where s = getSize . size $ m
         rs = getSize . size $ rm

indexJ i (Append m l@(Single _ lx) r@(Single _ rx))
        | i == 0 = Just lx
        | i == 1 = Just rx
        | otherwise = Nothing

