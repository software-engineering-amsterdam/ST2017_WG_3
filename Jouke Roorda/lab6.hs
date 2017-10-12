import Lecture6 hiding (exM,composites)
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
