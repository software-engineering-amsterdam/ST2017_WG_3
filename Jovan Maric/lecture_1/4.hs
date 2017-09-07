module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 38 mins

reversal :: Integer -> Integer
reversal = read . reverse . show

check_inverse :: Integer -> Bool
check_inverse n = prime n && prime (reversal n)

find_rev_primes :: Integer -> [Integer]
find_rev_primes n = filter (check_inverse) [1..n]

-- Answer
-- Testing reversal should be done by checking if the reversal of the reversal
-- gives the initial natural number back.

q_test5 = quickCheckResult (\n -> n >= 1 --> reversal (reversal n) == n)

-- The above gives a number, as reversal of a string does not uphold the same
-- way as in integers. For instance 10 reversed is 01. 01 is when cast to an int:
-- simply 1. Therefore the re-reversal of 10 will not return 10, but 1. This can
-- be solved in this context by checking if 10 is prime.

q_test6 = quickCheckResult (\n -> prime n --> reversal (reversal n) == n)
