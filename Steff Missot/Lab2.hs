module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

-- #####################################################################################################################
-- Lab Assignment 1
-- Amount of time taken: 1 hour (preparation Haskell IO / Monad)
-- #####################################################################################################################

-- | get all the elements in the list between min max
splitOnMinMax :: [Float] -> Float -> Float -> [Float]
splitOnMinMax list min max = (filter (\n -> n >= min && n < max) list)

checkQuartiles input = do
                    list <- input
                    let first = length (splitOnMinMax list 0.0 0.25)
                    let second = length (splitOnMinMax list 0.25 0.5)
                    let third = length (splitOnMinMax list 0.5 0.75)
                    let fourth = length (splitOnMinMax list 0.75 1.0)
                    return [(1, first), (2,second), (3,third), (4,fourth)]

assignment1 = checkQuartiles (probs 10000)
-- The output:
-- [(1,2383),(2,2639),(3,2491),(4,2487)]
-- [(1,2534),(2,2457),(3,2513),(4,2496)]
-- [(1,2499),(2,2502),(3,2512),(4,2487)]
-- [(1,2544),(2,2534),(3,2436),(4,2486)]
-- [(1,2509),(2,2543),(3,2494),(4,2454)]

-- We tested the amount of numbers in each Quartile.
-- We encounter roughly 2500 in each quartile
--
-- The differences are therefore minimal and this function can be considered random
-- To be sure I ran the test with a larger number of probs, in this case a million (1.000.000)
-- The result is as follows:
-- [(1,250363),(2,249637),(3,249608),(4,250392)]
-- [(1,249816),(2,250341),(3,249867),(4,249976)]
-- [(1,250082),(2,249393),(3,250344),(4,250181)]
-- We can conclude that the differences are much smaller compared to running it with 10.000 elements.
-- This builds a stronger case in proving that this function is random.


-- #####################################################################################################################
-- Lab Assignment 2
-- Recognizing triangles
-- Amount of time taken: 1 hour
-- #####################################################################################################################

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- 1. Haskell Program
triangle :: Integer -> Integer -> Integer -> Shape
triangle x' y' z' = let sorted = sort [x',y',z']
                        x = (head sorted)
                        y = (head (drop 1 sorted))
                        z = (last sorted)
                    in if x + y <= z then NoTriangle
                    else if x == y && y == z then Equilateral
                    else if x^2 + y^2 == z^2 then Rectangular
                    else if x == y || x == z || z == y then Isosceles
                    else Other

-- 2. Concise Test Report
testNoTriangle = (triangle 1 10 5) == NoTriangle
testEquilateral = (triangle 5 5 5) == Equilateral
testRectangular = (triangle 3 4 5) == Rectangular
testIsosceles = (triangle 5 3 5) == Isosceles
testOther = (triangle 4 5 6) == Other

-- Here we test if each If/Else block can be executed if passed the right values.
-- We have proven this is the case since all 5 of the above tests return true

-- We can also test if our let function works as expected:
testTriangleSort = (triangle 1 10 5) == (triangle 10 5 1) && (triangle 10 5 1) == (triangle 10 1 5)

-- #####################################################################################################################
-- Lab Assignment 3
-- Testing properties strength
-- Amount of time taken: 1 hour
-- #####################################################################################################################

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

ass31 = (\ x -> even x && x > 3)
-- Return True since the left part is more specific then even because of the AND operator
ass31Test = stronger [-10..10] ass31 even

ass32 = (\ x -> even x || x > 3)
-- Returns False since an OR statement is used, which makes (even && x>3) stronger then (even)
ass32Test = stronger [-10..10] ass32 even

ass33 = (\ x -> (even x && x > 3) || even x)
-- Returns True since (even == even) so the AND statement can be completely ignored.
-- and (even x && x > 3) is stronger then (even)
ass33Test = stronger [-10..10] ass33 even

ass34 = (\ x -> (even x && x > 3) || even x)
-- Returns True since (even == even) so the AND statement again, can be ignored
ass34Test = stronger [-10..10] even ass34

-- Descending Strength list:
-- ass31 is the strongest
-- ass33 and ass34 are even strong
-- ass32


-- #####################################################################################################################
-- Lab Assignment 4
-- Recognizing Permutations
-- Amount of time taken: 1 hour
-- #####################################################################################################################

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] xs  = False
isPermutation (x:xs) ys = elem x ys && isPermutation xs (delete x ys)

pEqual :: Eq a => [a] -> Bool
pEqual xs = isPermutation xs xs
-- quickCheck pEqual
-- +++ OK, passed 100 tests.

pReversed :: Eq a => [a] -> Bool
pReversed xs = (isPermutation xs (reverse xs))
-- quickCheck pReversed
-- +++ OK, passed 100 tests.

pSorted :: Ord a => [a] -> Bool
pSorted xs = isPermutation xs (sort xs)
-- quickCheck pSorted
-- +++ OK, passed 100 tests.


-- #####################################################################################################################
-- Lab Assignment 5
-- Recognizing and generating derangements
-- Amount of time taken: 1 hour
-- #####################################################################################################################

-- #####################################################################################################################
-- Lab Assignment 6
-- Implementing and testing ROT13 encoding
-- Amount of time taken:
-- #####################################################################################################################

-- #####################################################################################################################
-- Lab Assignment 7
-- Implementing and testing IBAN validation
-- Amount of time taken: 
-- #####################################################################################################################