-- All arguments are mapped to absolute values
-- This means we do not completely ignore invalid arguments

import Data.List
import Test.QuickCheck

-- Exercise 1.1 - 1h --

ind1, ind2 :: Int -> Int
ind1 = \ n -> if n > 0 then n^2 + (ind1 $ n-1) else 0
ind2 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

funcCompare :: (Int -> Int) -> (Int -> Int) -> Int -> Bool
funcCompare = \ p1 p2 x -> p1 x == p2 x

inductionTest1 :: [Int] -> Bool
inductionTest1 = \ xs -> all (funcCompare ind1 ind2) $ map abs xs

-- Exercise 1.2 - 15m --
ind3, ind4 :: Int -> Int
ind3 = \ n -> if n > 0 then n^3 + (ind3 $ n-1) else 0
ind4 = \ n -> (n*(n+1) `div` 2)^2

inductionTest2 :: [Int] -> Bool
inductionTest2 = \ xs -> all (funcCompare ind3 ind4) $ map abs xs

-- Exercise 2 - 30m --

ind5, ind6 :: Int -> Int
ind5 = \ n -> length $ subsequences [1..n]
ind6 = \ n -> 2^n

inductionTest3 :: [Int] -> Bool
inductionTest3 = \ xs -> all (funcCompare ind5 ind6) $ map abs xs

-- This is relatively hard to test as the number of subsets
-- grows exponentially. 
-- It's testing you testing whether subsequences satisfies
-- a part of its specification



