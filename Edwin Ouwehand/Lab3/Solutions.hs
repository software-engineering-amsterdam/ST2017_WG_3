
module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- Exercise 1 (1h)
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

---- | logical entailment 
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)
--entails (Impl p q) (Impl (Neg q) (Neg p))

---- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)


-- Exercise 2
