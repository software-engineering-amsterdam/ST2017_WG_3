-- 17.33 - 17.46

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

-- Write a function isPermutation

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l1 l2 = elem l1 (permutations l2)

-- Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?

testEqual :: Eq a => [a] -> Bool
testEqual l1 = isPermutation l1 l1
-- quickCheck testEqual

testReverse :: Eq a => [a] -> Bool
testReverse l1 = isPermutation l1 (reverse l1)
-- quickCheck testReverse

-- Provide an ordered list of properties by strength using the weakear and stronger definitions.
-- ???

-- Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.
-- ???
