-- 18.01 - 18:17

module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture2
import Lecture6 hiding (composites)

-- Only which are not prime are composites. However 0 and 1 cant be a composite,
-- 2 is prime, 3 is prime and 4 is therefore the first composites number.
composites :: [Integer]
composites = filter (not . prime) [4..]
