{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import System.IO.Unsafe

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

losses :: [DieValue] -> [DieValue] -> (Int, Int)
losses atkRolls defRolls = (length . filter (==True) $ cmp, length . filter (==False) $ cmp)
    where atk = sort atkRolls
          def = sort defRolls
          cmp = zipWith (\a d -> a > d) atk def

rollN :: Int -> Rand StdGen [DieValue]
rollN n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atk def) = do
    atkRolls <- rollN (max atk 3)
    defRolls <- rollN (max def 2)
    let (atkLost, defLost) = losses atkRolls defRolls
    return (Battlefield (atk - atkLost) (def - defLost))

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield atk def) 
    | atk < 2 || def < 1 = return b
    | otherwise = (battle b) >>= invade

countVictories :: [Battlefield] -> Rand StdGen Integer
countVictories bs = return $ foldr (\(Battlefield a d) count -> if a > 1 then (count + 1) else count) 0 bs

successProb :: Battlefield -> Rand StdGen Double
successProb b = invasions >>= countVictories >>= calcOdds
    where invasions = replicateM 1000 (invade b)
          calcOdds i = return ((fromInteger i) / 1000)

main :: IO ()
main = do
    prob <- evalRandIO (successProb (Battlefield 30 20))
    putStrLn (show prob)
