module Employee where

{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Tree
import           Data.Monoid

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

testGL1 = GL [(Emp "Stan" 9), (Emp "Bob" 3), (Emp "John" 1)] 13

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fs) = GL (e:es) (fs + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fs1) gl2@(GL _ fs2) 
  | fs1 > fs2 = gl1
  | otherwise = gl2

instance Monoid (GuestList) where
  mempty = GL [] 0
  mappend (GL es1 fs1) (GL es2 fs2) = GL (es1 ++ es2) (fs1 + fs2)

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a ts) = f a (map (treeFold f) ts)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLists = (maximumS withBossL, maximumS withoutBossL)
  where withoutBossL   = map fst bestLists
        withoutSubBoss = map snd bestLists
        withBossL      = map (glCons boss) withoutSubBoss

maximumS [] = mempty
maximumS xs = maximum xs

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel
