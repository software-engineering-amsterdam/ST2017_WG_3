module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- #####################################################################################################################
-- Lab Assignment 1
-- Amount of time taken: 1.5 hours
-- #####################################################################################################################

contradiction :: Form -> Bool
contradiction a = not (satisfiable a)
-- Test:
-- contradiction (Equiv (Neg p) p) == True
-- contradiction (Equiv p p) == False

tautology :: Form -> Bool
tautology a = all (\ v -> evl v a) (allVals a)
-- Test:
-- tautology (Dsj [p, Neg p]) == True
-- tautology (Dsj [p, p]) == False
-- tautology (Dsj [Neg p, Neg p]) == False
-- tautology (Equiv (Impl p q) (Impl (Neg q) (Neg p))) == True

-- | logical entailment
entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)
-- Test
-- entails (Impl p q) (Impl (Neg q) (Neg p)) == True
-- p -> q === ~q -> p

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b)


-- I have checked the definitions by providing examples from truth tables. to see the outcome

-- #####################################################################################################################
-- Lab Assignment 2
-- Amount of time taken:  hours
-- #####################################################################################################################

