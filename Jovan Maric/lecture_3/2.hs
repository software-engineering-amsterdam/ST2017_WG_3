-- 18:30 - 19:05

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

parseToStr :: String -> String
parseToStr =  show . head . parse

test1 = parseToStr "*(1 +(2 -3))" == "*(1 +(2 -3))" -- True

-- Drops the other )) as they are not needed.
test3 = parseToStr "*(1 +(2 -3))))" == "*(1 +(2 -3))" -- True
