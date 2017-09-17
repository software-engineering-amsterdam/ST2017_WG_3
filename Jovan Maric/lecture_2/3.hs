-- 17.10 -- 17.33

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

domain = [(-10)..10]

-- Workshop 2.3
prop1, prop2, prop3, prop4 :: Integer -> Bool
prop1 = (\ x -> even x && x > 3)
prop2 = (\ x -> even x || x > 3)
prop3 = (\ x -> (even x && x > 3) || even x)
prop4 = even

test1 = stronger domain prop1 even
test2 = stronger domain prop2 even
test3 = stronger domain prop3 even
test4 = stronger domain even (\ x -> (even x && x > 3) || even x)
