import Data.List
import Data.Char
import Data.Fixed
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

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

-- Exercise 2 - 30m --

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

-- Exercise 3 - 1h --

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

p1, p2, p3, p4 :: Int -> Bool
p1 = (\ x -> even x && x > 3)
p2 = (\ x -> even x || x > 3)
p3 = (\ x -> (even x && x > 3) || even x)
p4 = even

properties = [("p1", p1), ("p2", p2), ("p3", p3), ("p4", p4)]

getName (n, _) = n
getFunc (_, f) = f

-- quicksort :: Ord a => [a] -> [a]  
-- quicksort :: [Int] -> [String, (Int -> Bool)] -> [(String, Int)]
quicksort' _ [] = []  
quicksort' r (x:xs) = 
   quicksort' r [ a | a <- xs, stronger r (getFunc x) (getFunc a)]
   ++ [x]
   ++ quicksort' r [ a | a <- xs, not $ stronger r (getFunc x) (getFunc a)]

-- When using stronger and weaker, p4 will occur twice as it is as strong as p3,
-- given that the "&& x > 3" is useless with the "|| even x" in place

lstxx = map getName $ quicksort' [-10..10] properties

-- Exercise 4 - 1h --
-- https://stackoverflow.com/a/29307068
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

teq (x, y) = x == y

infix 1 ^^^
(^^^) :: (a -> b) -> (a, a) -> (b, b)
f ^^^ (y, z) = (f y, f z)

isPermutation' :: Eq a => ([a], [a]) -> [Bool]
isPermutation' ([], []) = [True]
isPermutation' (xs, []) = [False]
isPermutation' ([], ys) = [False]
isPermutation' (xs, ys) = (teq $ count (head xs) ^^^ (xs, ys)):(isPermutation' $ (filter (/= head xs)) ^^^ (xs, ys))

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = all (==True) $ isPermutation' (xs, ys)

