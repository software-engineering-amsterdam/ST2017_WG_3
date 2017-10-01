-- 20:20 - 22:08

module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck
import SetOrd
import Lecture2
import Lecture4

{-
  Use the datatype for relations from the previous exercise, plus
  > infixr 5 @@
  >
  > (@@) :: Eq a => Rel a -> Rel a -> Rel a
  > r @@ s =
  >   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
  to define a function

   trClos :: Ord a => Rel a -> Rel a
  that gives the transitive closure of a relation, represented as an ordered list of pairs. E.g., trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].

  (Deliverable: Haskell program, indication of time spent.)
-}

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

combin :: Rel a -> Rel a
combin xs = concat [ [x, y] | x <- xs, y <- xs ]

trClos' :: Rel a -> Rel a
trClos' (x:xs) = (x @@ head (tail xs)) : trClos' xs

-- NOT YET FInished, didn't know how to solve this.
