-- 1 hour, 42 mins

import Data.Char
import Test.QuickCheck

reversal :: Integer -> String
reversal = reverse . show

sum_int_list :: String -> Int
sum_int_list s = sum (map (digitToInt) s)

doubling :: Char -> Int
doubling s | n > 9     =  sum_int_list (show n)
           | otherwise  = n - 9
           where n      = digitToInt s * 2

calculate :: String -> Int
calculate [] = 0
calculate [x] = doubling x
calculate (x:y:xs) = doubling y + calculate xs

luhn :: Integer -> Bool
luhn n = calculate (reversal n) `mod` 10 == 0

list_to_int :: [Int] -> Int
list_to_int l = read (map (intToDigit) l) :: Int

-- Creditcard properties from: http://www.regular-expressions.info/creditcard.html
is_american_express, is_master, is_visa :: Integer -> Bool
is_american_express n = (length ns == 15)
                        && elem (list_to_int (take 2 ns)) [34, 37]
                        && luhn n
                        where ns = map (digitToInt) (show n)

is_master n = (length ns == 16)
              && elem (list_to_int (take 2 ns)) [51..55]
              && elem (list_to_int (take 4 ns)) [2221..2720]
              && luhn n
              where ns = map (digitToInt) (show n)

is_visa n = (length ns == 16 || length ns == 13)
            && head ns == 4
            && luhn n
            where ns = map (digitToInt) (show n)

-- Warning!! I have no idea how to apply quickCheck on this, how to let quickCheck
-- grab from a list of creditcard numbers...
