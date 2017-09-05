-- 1h
-- All arguments are mapped to absolute values
-- This means we do not completely ignore invalid arguments

import Test.QuickCheck

ind1, ind2 :: Int -> Int
ind1 = \ n -> if n > 0 then n^2 + (ind1 $ n-1) else 0
ind2 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

funcCompare :: (Int -> Int) -> (Int -> Int) -> Int -> Bool
funcCompare = \ p1 p2 x -> p1 x == p2 x

inductionTest :: [Int] -> Bool
inductionTest = \ xs -> all (funcCompare ind1 ind2) (map abs xs)
