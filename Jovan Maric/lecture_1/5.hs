module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

-- 30 mins
sum_primes :: [Integer] -> Integer
sum_primes xs = sum (take 100 xs)

lowest_prime :: [Integer] -> Integer
lowest_prime (x:xs) | prime ((sum_primes xs) + x) = x
                    | otherwise                   = lowest_prime xs

-- Answer
-- The lowest_prime can be tested by checking if the endresult is prime and
-- by checking that the lowest_prime + 100 next primes in the sequel summed up
-- are prime.
