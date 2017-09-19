-- 17.50 - 18.19

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

-- -- First, give a specification of ROT13.

-- ROT13 = A cipher by rotating the alphabet 13 characters. So 'a' = 'n', 'b' = 'o'
-- etc.

-- -- Next, give a simple implementation of ROT13.

move13lower, move13upper :: Char -> Char
move13lower c = chr ((((ord c - 97 + 13)) `mod` 26) + 97)
move13upper c = chr ((((ord c - 65 + 13)) `mod` 26) + 65)

transform :: String -> String
transform [] = []
transform (c:cs) | ord c >= 65 && ord c < 90   = move13upper c : transform cs
                 | ord c >= 97 && ord c < 122  = move13lower c : transform cs
                 | otherwise                   = c : transform cs

rot13 :: String -> String
rot13 s = transform s

-- Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.
-- -- Test inverse
test1 = rot13 (rot13 "HENK") == "HENK"
quickCheck test1

-- -- Test appliance only on letters.
test2 = rot13 "!@#$%^&*()1234567890-=" == "!@#$%^&*()1234567890-="
