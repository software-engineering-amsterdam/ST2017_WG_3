-- 22.40 - 23:40

module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6

false_pos :: [Integer] -> Int -> IO ()
false_pos [] k     = print "Nein"
false_pos (x:xs) k = do
  p <- primeTestsF k x
  if p then
    print x
  else
    false_pos xs k

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
   k <- [2..],
   prime (6*k+1),
   prime (12*k+1),
   prime (18*k+1) ]

-- ## Usage
test = false_pos carmichael 1

-- The test function returns 294409 much more often than 56052361. There could
-- be more alternative numbers, but by manual testing these haven't yet been
-- found. The numbers can be explained by that 56052361 is composed of 294409.
-- By looking at https://en.wikipedia.org/wiki/Carmichael_number, ????????????????????????????????
