
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


-- Exercise 2 (1h)
data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rightangled | Other deriving (Eq,Show)

-- Not a triangle   when the length of one side is longer than the others combined. (Any other cases?)
-- Equilateral      when all sides are equal.
-- Rightangled      when one corner is exactly 90 degrees.
-- Isosceles        when not equilateral and has two sides of equal length. 
-- other            when none of the above but still a triangle. 

isTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isTriangle x y z = x < (y + z)

isEquilateral :: Eq a => a -> a -> a -> Bool
isEquilateral x y z = x == y && y == z

-- We know that pythagoras holds only for rightangled triangles,
-- and since we know all three sides we can verify it this way. 
isRightangled :: (Eq a, Num a) => a -> a -> a -> Bool
isRightangled x y z = (y^2 + z^2) == x^2

isIsosceles :: Eq a => a -> a -> a -> Bool
isIsosceles x y z = not (isEquilateral x y z) && (x == y || y == z || x == z)

isOther :: (Num a, Ord a) => a -> a -> a -> Bool
isOther x y z = isTriangle x y z && not (isEquilateral x y z) &&  not (isRightangled x y z) && not (isIsosceles x y z)

shape :: (Num a, Ord a) => [a] -> Shape
shape [x, y, z] | not (isTriangle x y z) = NoTriangle
                | isEquilateral x y z    = Equilateral
                | isRightangled x y z    = Rightangled
                | isIsosceles x y z      = Isosceles
                | isOther x y z          = Other

triangle :: Int -> Int -> Int -> Shape
triangle x y z = shape (reverse (sort[x, y, z]))


-- Exercise 3 (30m)
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

-- a) Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type Int -> Bool. Consider a small domain like [(−10)..10]
-- b) Provide a descending strength list of all the implemented properties.

domain = [(-10)..10]

-- rewrite so that :: Int -> Bool
worksh1 = stronger domain (\ x -> even x && x > 3) even
worksh2 = stronger domain (\ x -> even x || x > 3) even
worksh3 = stronger domain (\ x -> (even x && x > 3) || even x) even
worksh4 = stronger domain even (\ x -> (even x && x > 3) || even x) 
