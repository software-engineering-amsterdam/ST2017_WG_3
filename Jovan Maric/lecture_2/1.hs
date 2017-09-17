-- 15.30 - 16:30

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1)
            return (p:ps)

quarter, quarterToHalve, halveToThreeQuart, threeQuartToOne :: [Float] -> Int
quarter fs = length (filter (< 0.25) fs)
quarterToHalve fs = length (filter (\x -> x >= 0.25 && x < 0.5) fs)
halveToThreeQuart fs = length (filter (\x -> x >= 0.5 && x < 0.75) fs)
threeQuartToOne fs = length (filter (\x -> x >= 0.75 && x < 1) fs)

countProbs :: [Float] -> [Int]
countProbs fs = [quarter fs, quarterToHalve fs, halveToThreeQuart fs, threeQuartToOne fs]

inRange :: [Int] -> String
inRange xs = "Are all values of list " ++ show xs ++ " >= 2400 && <= 2600: "
              ++ show (all (\x -> x >= 2400 && x <= 2600) xs)

check = do
  xs <- probs 10000
  return (inRange (countProbs xs))
