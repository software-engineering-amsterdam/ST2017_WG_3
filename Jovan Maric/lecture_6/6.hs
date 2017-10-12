-- 23.40 - ??

module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
   k <- [2..],
   prime (6*k+1),
   prime (12*k+1),
   prime (18*k+1) ]

cm_test :: [Integer] -> Int -> IO ()
cm_test [] k     = print "Nein"
cm_test (x:xs) k = do
  p <- primeMR k x
  if p then
    print x
  else
    cm_test xs k

-- Still quite a lot of false_positives when using primeMR. Even more than with
-- the test in 5.hs.
