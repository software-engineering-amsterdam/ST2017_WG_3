-- 17:20 - 18:30

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- If any evaluates to true
-- satisfiable :: Form -> Bool
-- satisfiable f = any (\v -> evl v f) (allVals f)

-- If all evaluate to false
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- If all evaluate to true
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- If and only if all elements of B are true then A is true
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- If A equals B
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- Tests
test1 = contradiction (Equiv p (Neg p))
test2 = tautology (Equiv p p)
test3 = entails p p
test4 = equiv p p
