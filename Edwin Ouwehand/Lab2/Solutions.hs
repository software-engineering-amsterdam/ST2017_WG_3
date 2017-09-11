
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1) 
            return (p:ps)


-- Exercise 1 (1h30m)
splitQuartile :: Float -> Float -> [Float] -> [Float]
splitQuartile lo hi list = filter (\x -> x>=lo && x<=hi) list

calcFrequency :: IO [Int]
calcFrequency = do 
    xs <- probs 10000
    let q1 = splitQuartile 0 0.25 xs
        q2 = splitQuartile 0.25 0.50 xs
        q3 = splitQuartile 0.50 0.75 xs
        q4 = splitQuartile 0.75 1 xs  in 
        return [length q1, length q2, length q3, length q4]


-- Exercise 2 
isTriangle = 

--rightangled triangle
