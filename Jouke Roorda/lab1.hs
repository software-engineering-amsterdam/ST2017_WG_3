-- All arguments are mapped to absolute values
-- This means we do not completely ignore invalid arguments

import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
-- It hardly makes sense to use the even numbers too
primes = 2 : filter prime [3,5..]

-- Exercise 1.1 - 1h --

ind1, ind2 :: Int -> Int
ind1 = \ n -> if n > 0 then n^2 + (ind1 $ n-1) else 0
ind2 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

funcCompare :: (Int -> Int) -> (Int -> Int) -> Int -> Bool
funcCompare = \ p1 p2 x -> p1 x == p2 x

inductionTest1 :: [Int] -> Bool
inductionTest1 = \ xs -> all (funcCompare ind1 ind2) $ map abs xs

-- Exercise 1.2 - 15m --

ind3, ind4 :: Int -> Int
ind3 = \ n -> if n > 0 then n^3 + (ind3 $ n-1) else 0
ind4 = \ n -> (n*(n+1) `div` 2)^2

inductionTest2 :: [Int] -> Bool
inductionTest2 = \ xs -> all (funcCompare ind3 ind4) $ map abs xs

-- Exercise 2 - 30m --

ind5, ind6 :: Int -> Int
ind5 = \ n -> length $ subsequences [1..n]
ind6 = \ n -> 2^n

inductionTest3 :: [Int] -> Bool
inductionTest3 = \ xs -> all (funcCompare ind5 ind6) $ map abs xs

-- This is relatively hard to test as the number of subsets
-- grows exponentially. 
-- It's testing you testing whether subsequences satisfies
-- a part of its specification

-- Exercise 3 - 15m --

ind7, ind8 :: Int -> Int
ind7 = \ n -> length $ permutations [1..n]
ind8 = \ n -> product [1..n]

inductionTest4 :: [Int] -> Bool
inductionTest4 = \ xs -> all (funcCompare ind7 ind8) $ map abs xs

-- Exercise 4 - 30m --

reversal :: Integer -> Integer
reversal = read . reverse . show

-- You would be able to write your own reversal function,
-- e.g. by dividing each number by ten and appending it using recursion
-- A very basal example would be:
-- x = \ y -> (y `mod` 10):(x y)
-- You would only have to reconstruct the number again

prim :: [Integer]
prim = takeWhile (\ n -> n < 10000) $ filter (prime.reversal) primes

-- Exercise 5 - 15m --

primsum' :: [Integer] -> Integer
primsum' xs = if prime.sum $ take 101 xs then sum $ take 101 xs else primsum' $ tail xs

primsum :: Integer
primsum = primsum' primes

-- Exercise 6 -  --

refcon :: [([Integer], Integer)]
refcon = [(ys, product ys + 1) | ys <- [take n primes | n <- [2..]], not.prime $ product ys + 1]

smallestrc = head refcon

