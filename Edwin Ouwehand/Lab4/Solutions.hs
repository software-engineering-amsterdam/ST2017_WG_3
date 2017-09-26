
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd
import Lecture4

-- Exercise 1
-- Russell
-- 4.17

-- Exercise 2 (2h)
-- Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs. 
-- First do this from scratch, next give a version that uses QuickCheck to random test this datatype.
-- (Deliverables: two random test generators, indication of time spent.)

-- TODO custom generator. 

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
            xs <- arbitrary
            return (Set $ sort $ nub xs)

prop_isEmpty_empty :: Bool
prop_isEmpty_empty = isEmpty emptySet

prop_member_empty :: Int -> Bool
prop_member_empty x = not (inSet x emptySet)

prop_isEmpty_insert :: Int -> Set Int -> Bool
prop_isEmpty_insert x s = not (isEmpty (insertSet x s))

prop_member_delete :: Int -> Set Int -> Bool
prop_member_delete x s = not (inSet x (deleteSet x s))


-- Exercise 3
-- Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. Next, 
-- use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.
-- (Deliverables: implementations, test properties, short test report, indication of time spent.)

setIntersection :: Ord a => Set a -> Set a-> Set a
setIntersection x y = list2set $ intersect (set2List x) (set2List y)

setUnion :: Ord a => Set a -> Set a-> Set a
setUnion x y = list2set $ sort $ nub $ merge (set2List x) (set2List y)

setDifference :: Ord a => Set a -> Set a-> Set a
setDifference x y = list2set ((set2List x) \\ (set2List y))

setSymDifference :: Ord a => Set a -> Set a-> Set a
setSymDifference x y = list2set $ merge ((set2List x) \\ (set2List y)) ((set2List y) \\ (set2List x))

-- Merge function 
-- Src: https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

set2List :: Set a -> [a]
set2List (Set xs) = xs

prop_union_subset :: Set Int -> Set Int -> Bool
prop_union_subset x y = subSet x z && subSet y z
    where z = (setUnion x y)

prop_difference_inset :: Set Int -> Set Int -> Bool
prop_difference_inset x y = not (any (\ z -> inSet z (setDifference x y)) (set2List y))
--inSet

--report & timespent 


-- Exercise 4

