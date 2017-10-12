-- 21.10 - 22:20

module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6

{--
  The function can be executed as: false_pos composites k, where k is from my
  understanding the amount of candidates that need to be generated for
  primeTestsF to construct a random number. primeTestsF is simply used to check
  if an list element provided by composites triggers a false positive,
  proclaiming the primeTestsF function not bugfree.
--}

false_pos :: [Integer] -> Int -> IO ()
false_pos [] k     = print "Nein"
false_pos (x:xs) k = do
  p <- primeTestsF k x
  if p then
    print x
  else
    false_pos xs k
