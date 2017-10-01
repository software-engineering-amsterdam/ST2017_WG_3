-- 17:20 - 19:30

module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Lecture2
import Lecture4

{-
Implement a random data generator for the datatype Set Int, where Set is as
defined in SetOrd.hs. First do this from scratch, next give a version that uses
QuickCheck to random test this datatype.

(Deliverables: two random test generators, indication of time spent.)
-}

-- # Scratch version
-- SetOrd defines a function which turns a list into a set and orders them
-- therefore it is easy to generate an ordered set from a list by using
-- a random Int list generator located in Lecture2.

genSet :: IO (Set Int)
genSet = do
  ints <- genIntList
  return (list2set ints)

-- # QuickCheck version
-- # Taken from Steff as nothing I tried worked
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
            xs <- arbitrary
            return (Set $ nub $ sort $ xs)

-- Atleast I have a generator
randomSet = generate (arbitrary :: Gen (Set Int))
