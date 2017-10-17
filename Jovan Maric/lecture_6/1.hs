-- 14:00 - 17:30
module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6 hiding (exM)

{--
  When the exponent is even, then by means of modulo squaring, x^y `mod` n can
  be reduced to (x^(y `div` 2) * x^(y `div` 2)) `mod` n because (a^2 * a^3) = a^(2+3) = a^5.
  By reducing the exponentiation by means of divide and conquer, it results in
  a less memory intensive and easier expression. Therefore making the final modulo
  less intensive. https://en.wikipedia.org/wiki/Modular_exponentiation
--}
exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM x y n | even y    = dup_exM `mod` n
          | otherwise = (x * dup_exM) `mod` n
        where
          exM' = exM x (y `div` 2) n
          dup_exM = exM' * exM'
