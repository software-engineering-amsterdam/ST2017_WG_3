module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 30 mins

-- Workshop 4
card' :: Int -> Int
card' n = length (subsequences [0..n])

pow' :: Int -> Int
pow' n = 2^(length [0..n])

q_test3 = quickCheckResult (\n -> n >= 0 --> card' n == pow' n)

-- Answer 1:
-- Its hard to test because the subsequences increases exponentially the more
-- combinations a subset contains. Therefore the first few lists can be easily
-- generated as their length is small. But as quickCheck increases the testing
-- lists, it gets tougher to generate the combinations and therefore the program
-- gets slower.

-- Answer 2:
-- You test for both the subsequences specification and the induction of A
-- as they can both be tested in the same manner.
