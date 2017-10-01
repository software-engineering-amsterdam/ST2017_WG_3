-- 20:00 - 21:25

module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Lecture2
import Lecture4

-- From 2.hs
genSet :: IO (Set Int)
genSet = do
  ints <- genIntList
  return (list2set ints)

{-
Implement operations for set intersection, set union and set difference, for the
datatype Set defined in SetOrd.hs. Next, use automated testing to check that
your implementation is correct. First use your own generator, next use
QuickCheck.

(Deliverables: implementations, test properties, short test report, indication
of time spent.)
-}

-- For all following functions, the Data.List operators are used and afterwards
-- converted to a set. https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-List.html

setIntersect :: Ord a => Set a -> Set a -> Set a
setIntersect (Set a) (Set b) = list2set $ (intersect a b)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = list2set $ (a \\ b)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set $ union a b

setToList :: Ord a => Set a -> [a]
setToList (Set a) = a

-- Intersect test
testIntersectSet = setIntersect (Set [2..4]) (Set [3..8])
testIntersectList = sort $ (intersect [2..4] [3..8])
testIntersect = (setToList testIntersectSet) == testIntersectList
-- True

-- Difference test
testDifferenceSet = setDifference (Set [2..4]) (Set [3..8])
testDifferenceList = sort $ ([2..4] \\ [3..8])
testDifference = (setToList testDifferenceSet) == testDifferenceList
-- True

-- setUnion
testUnionSet = setUnion (Set [2..4]) (Set [3..8])
testUnionList = sort $ (union [2..4] [3..8])
testUnion = (setToList testUnionSet) == testUnionList
-- True
