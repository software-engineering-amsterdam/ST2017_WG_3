module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd
import Data.Tuple


-- #####################################################################################################################
-- Lab Assignment 1
-- Amount of time taken: 2 hours
-- #####################################################################################################################


-- On page 126 Exercise 4.10 it asks the following:
-- Show:
-- {a1, a2} = {b1, b2} iff: a1 = b1 ∧ a2 = b2, or a1 = b2 ∧ a2 = b
-- How does this work?

-- The empty symbol is used to check if a set is empty:
-- <empty> is a subset of A for example.
-- But if something(empty) is a subset of another set(A), how come that set is empty?
-- Clearly, <empty> is in it.
-- To me it seems like set A is a singleton with <empty> in it, and therefor not empty since the
-- cardinality of that set is > 0
-- Empty is a property, not an element, right?

-- #####################################################################################################################
-- Lab Assignment 2
-- Implement a random data generator for the datatype Set Int,
-- where Set is as defined in SetOrd.hs. First do this from scratch,
-- next give a version that uses QuickCheck to random test this datatype.
-- Amount of time taken:
-- #####################################################################################################################


-- #####################################################################################################################
-- Lab Assignment 3
-- Amount of time taken:  hours
-- #####################################################################################################################


-- #####################################################################################################################
-- Lab Assignment 4
-- Amount of time taken:  hours
-- #####################################################################################################################

-- #####################################################################################################################
-- Lab Assignment 5
-- Amount of time taken: 20 minutes
-- #####################################################################################################################

type Rel a = [(a,a)]

-- By using concatMap, I can return more items then are in the present list
-- I have included the swap method from Data.Tuple which swaps the tuple around
symClos :: Ord a => Rel a -> Rel a
symClos xs = concatMap (\n -> [n, swap n]) xs

-- symClos [(1,2),(2,3),(3,4)]
-- [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]

-- #####################################################################################################################
-- Lab Assignment 6
-- Amount of time taken:  hours
-- #####################################################################################################################


-- #####################################################################################################################
-- Lab Assignment 7
-- Amount of time taken:  hours
-- #####################################################################################################################


-- #####################################################################################################################
-- Lab Assignment 8
-- Amount of time taken:  hours
-- #####################################################################################################################
