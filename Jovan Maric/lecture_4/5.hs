-- 19:20 - 20:14

module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck
import SetOrd
import Lecture2
import Lecture4

{-
  Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. Assume the following definition:

  > type Rel a = [(a,a)]

  Use this to implement a function

  symClos :: Ord a => Rel a -> Rel a
  that gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].

  (Deliverable: Haskell program, indication of time spent.)
-}

type Rel a = [(a,a)]

-- Map (x,y) and (y,x) to an array then concat to list
symClos :: Ord a => Rel a -> Rel a
symClos xs = concat (map (\n -> [n, swap n]) xs)

testSymClos = symClos [(1,2),(2,3),(3,4)] == [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
-- True
