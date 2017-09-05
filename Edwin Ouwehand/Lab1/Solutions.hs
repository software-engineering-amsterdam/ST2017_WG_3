
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


-- Exercise 1 
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


-- Exercise 2
prodLen :: Int -> Bool
prodLen n = let len = length(subsequences [1 .. n]) in
                len == (2^n)

test3 = quickCheck(\n -> n>=0 --> prodLen n)


-- Exercise 3
fact n = product [1..n]

test4 = quickCheck (\n -> n>=0 --> length(permutations([1 .. n])) == fact n)


-- Exercise 4
reversablePrimes = filter (\x -> prime (reversal x)) (takeWhile (<10000) primes)

-- Test fails, since e.g. reversal 30 returns 3, thus information is lost. 
-- The function is usable in this context however, since this only applies to multitudes of 10, 
-- which are therefore never prime. 
testReversal = verboseCheckResult (\n -> n>=0 --> n == (reversal (reversal n)))


-- Exercise 5
primeSum from = let to = from + 101 in 
    sum (take (to - from) (drop from primes))

prime101 = head (filter(\x -> prime x) (map(\x -> primeSum x) [0..]))


-- Exercise 6
primeSet from range = let to = from + range in -- Should start running at 0, thus primeSet 0 x
    take (to - from) (drop from primes)

counterSet = head(filter(\x -> not (prime (product x + 1))) (map(\x -> primeSet 0 x) [0..]))
counterResult = product counterSet +1


-- Exercise 7
-- src: https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- src: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

--evenEl :: [Int] -> [Int]
--evenEl [] = []
--evenEl [x] = []
--evenEl (x:y:[]) = [y]
--evenEl (x:y:xs) = y:evenEl xs

oddEl :: [Integer] -> [Integer]
oddEl [] = []
oddEl [x] = []
oddEl (x:y:[]) = [x]
oddEl (x:y:xs) = x:oddEl xs

accountSum nr = sum (merge(oddEl nr) (map(\x -> if (x>=9) then sum (digs x) else x) (map(*2)(oddEl (reverse nr)))))

luhn :: Integer -> Bool
luhn nr = let aNr = take 10 (digs nr)
              check = drop 10 (digs nr) 
              total = (accountSum aNr) + (sum check) in
              mod total 10 == 0

