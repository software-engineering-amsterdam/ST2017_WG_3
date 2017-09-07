module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 40 mins

permu' :: Int -> Int
permu' n = length (permutations [1..n])

count_perms :: Int -> Int
count_perms 1 = 1
count_perms n = n * count_perms (n - 1)

q_test4 = quickCheckResult (\n -> n >= 0 --> permu' n == count_perms n)

-- Answer 1
-- The property is hard to test as like in the previous question, the permutations
-- increase exponentially. Low values for n are easy to generate in permu'.

-- Answer 2
-- Here permutations is tested against parts of its specification
