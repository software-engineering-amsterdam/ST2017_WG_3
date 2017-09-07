module Lab1 where
import Data.List
import Test.QuickCheck
import Lecture1

n_primes :: Int -> [Integer]
n_primes n = take n primes

prod_not_prime :: [Integer] -> Bool
prod_not_prime xs = not (prime ((product xs) + 1))

counter :: [[Integer]]
counter = take 1 [ xs | xs <- [n_primes n | n <- [2..]],
                        prod_not_prime xs]
