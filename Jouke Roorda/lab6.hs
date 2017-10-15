import Lecture6 hiding (exM,composites)
import Control.Monad.Extra
import Control.Monad.Loops
import Data.Bits

-- Exercise 1 - 1h --

zzz :: Integer -> Integer -> [Integer] -> Integer
zzz n z (x:xs) = zzz (n^2) z xs
zzz n z [] = n `mod` z

rsq p = takeWhile (<=p) $ map (2^) [1..p]

exM :: Integer -> Integer -> Integer -> Integer
exM n p z = l * n^l `mod` z where l = (zzz n z $ rsq p)


-- Exercise 3 - 2x30m --
composites :: [Integer]
-- Either implementation below works. The first implementation implements
-- every number larger than two by another number larger than two, thus
-- generating numbers by its definition. The downside of this is that some
-- odd numbers will in practice never show up because it will multiply every
-- natural number by two, which is already an infinite set, before multi-
-- plying everything by three.
-- The second implementation is simpler, by creating a set that complements
-- the set of prime numbers, which is another approach entirely.
-- It could be argued that you would need to have a very solid implementation
-- of the prime function.

-- composites = nub [x*y | x <- [2..], y <- [2..]]
composites = filter (not.prime) [2..]


-- Exercise 4 - 1h --
smallestM ll = do
    l <- ll
    return $ minimum $ concat l

-- After running this test with take 10000 composites, I noticed there being
-- some smaller than 1000 that were reported as a prime. As the task at hand
-- was to find the least composite number, taking the first 1000 composites
-- has to be sufficient. Additionally freeing up CPU time to do more passes
testPrimeTestsF = mapM (\z -> (filterM (primeTestsF 3) $ take 1000 composites)) [1..100]

-- After running 100 passes multiple times, the number 15 was returned
-- almost every time. However, at one occasion, 9 was returned.
-- In conclusion, nine is the smallest number we can find at this time.
-- However, knowing that 100 passes does not always returns the smalles number,
-- this does not mean nine is the smallest possible number this algorithm can find.
smallestFooled = smallestM testPrimeTestsF


-- Exercise 5 --

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]

