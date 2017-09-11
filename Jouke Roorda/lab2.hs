import Data.List
import Data.Char
import Data.Fixed
import System.Random
import Test.QuickCheck

-- Exercise 1 - 2h (mainly for test/avgDeviation) --

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
        p <- getStdRandom random
        ps <- probs (n-1)
        return (p:ps)

-- https://stackoverflow.com/a/26372259
sofrequency :: Ord a => [a] -> [Int]
sofrequency = map length.group.sort

-- dist :: Int -> IO [Int]
dist n = do
        ps <- probs n
        return $ sofrequency $ map (\ n -> n - (mod' n 0.25)) ps

avgDeviation' :: Int -> IO [Int]
avgDeviation' 0 = return []
avgDeviation' i = do
        dst <- dist 100000
        dstx <- avgDeviation' (i-1)
        return (((flip div 4).sum $ map (abs.(subtract 25000)) dst):dstx)

avgDeviation = do
        r <- avgDeviation' 100
        return $ (sum r) `div` (length r)

-- While not giving a perfect 1/4 per quarter distribution,
-- the avgDeviation function observes deviations around 100-115
-- on a 25000 numbers per quarter basis. This equals percentages
-- of 0.40-0.46%
probsExample = dist 100000

-- Exercise 2 --

data Shape = NoTriangle | Equilateral 
        | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
        | a <= 0 || b <= 0 || c <= 0 = NoTriangle
        | a == b && b == c           = Equilateral
        | a == b || b == c || a == c = Isosceles
        | a^2 + b^2 == c^2           = Rectangular
        | a^2 + c^2 == b^2           = Rectangular
        | b^2 + c^2 == a^2           = Rectangular
        | otherwise                  = Other

