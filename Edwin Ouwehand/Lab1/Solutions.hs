
module Lab1 where
import Data.List
import Test.QuickCheck 

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


-- Lab 1 exercise 1 
-- Workshop 2
eq1 :: Int -> Int 
eq1 = \ n -> sum( map (^2)[0 .. n])

eq2 :: Int -> Int
eq2 = \ n -> (n * (n + 1) * (2*n + 1)) `div` 6

--Always true for neg integers, actual test is ignored.
test1 = quickCheck(\n -> n>=0 --> (eq1 n == eq2 n)) 
test1Ver = verboseCheckResult(\n -> n>=0 --> (eq1 n == eq2 n)) 


-- Workshop 3
eq3 :: Int -> Int
eq3 = \ n -> sum( map (^3)[0 .. n])

eq4 :: Int -> Int
eq4 = \n -> ((n * (n + 1)) `div` 2)^2

test2 = quickCheck(\n -> n>=0 --> (eq3 n == eq4 n)) 
test2Ver = verboseCheckResult(\n -> n>=0 --> (eq3 n == eq4 n)) 


-- Lab 1 exercise 2
prodLen :: Int -> Bool
prodLen n = let len = length(subsequences [1 .. n]) in
                len == (2^n)

test3 = quickCheck(\n -> n>=0 --> prodLen n)


-- Lab 1 exercise 3
fact n = product [1..n]

test4 = quickCheck (\n -> n>=0 --> length(permutations([1 .. n])) == fact n)

-- Lab 1 exercise 4
--Take from the list of primes for 10000 times
reversablePrimes = filter (\x -> prime (reversal x)) (takeWhile (<10000) primes)

-- Test fails, since reversal 30 returns 3, thus information is lost. 
testReversal = verboseCheckResult (\n -> n>=0 --> n == (reversal (reversal n)))


-- Lab 1 exercise 5
