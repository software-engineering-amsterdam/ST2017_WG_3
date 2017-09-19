-- 16.30 - 17.10

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular
           | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | x + y <= z || y + z <= x || z + x <= y     = NoTriangle
               | x == y && y == z                           = Equilateral
               | x^2 + y^2 == z^2                           = Rectangular
               | x == y || y == z || x == z                 = Isosceles
               | otherwise                                  = Other

testData :: [[Integer]]
testData = [ [x,y,z] | x <- [0..5], y <- [x..5], z <- [x..5]]

tester :: [[Integer]] -> Shape -> [[Integer]]
tester xs s = [ [x, y, z] | [x, y, z] <- xs, triangle x y z == s]

-- -- NoTriangles
-- tester testData NoTriangle
--
-- -- Equilateral
-- tester testData Equilateral
--
-- -- Rectangular
-- tester testData Rectangular
--
-- -- Isosceles
-- tester testData Isosceles
--
-- -- Other
-- tester testData Other
