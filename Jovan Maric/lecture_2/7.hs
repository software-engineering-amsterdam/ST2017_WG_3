-- 18.20 - 19.15

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

-- WARNING: I haven't checked the IBAN formats by country. This can be done by
-- comparing the IBAN country code and validate with the list at https://en.wikipedia.org/wiki/International_Bank_Account_Number

removeSpaces :: String -> String
removeSpaces s = filter (/= ' ') s

moveFirst4ToEnd :: String -> String
moveFirst4ToEnd s = drop 4 s ++ take 4 s

charToInt :: Char -> String
charToInt c | elem (toUpper c) ['A'..'Z'] = show ([10..35] !! (ord (toUpper c) `mod` 65))
            | otherwise                   = c : []

replaceChars :: String -> String
replaceChars []     = []
replaceChars (c:cs) = charToInt c ++ replaceChars cs

strTransform :: String -> String
strTransform = replaceChars . moveFirst4ToEnd . removeSpaces

iban :: String -> Bool
iban s = ((read (strTransform s) :: Integer) `mod` 97) == 1

ibans = ["AL47 2121 1009 0000 0002 3569 8741", "AD12 0001 2030 2003 5910 0100", "AT61 1904 3002 3457 3201", "AZ21 NABZ 0000 0000 1370 1000 1944", "BH67 BMAG 0000 1299 1234 56", "BE62 5100 0754 7061", "BA39 1290 0794 0102 8494", "BG80 BNBG 9661 1020 3456 78", "HR12 1001 0051 8630 0016 0", "CY17 0020 0128 0000 0012 0052 7600", "CZ65 0800 0000 1920 0014 5399", "DK50 0040 0440 1162 43", "EE38 2200 2210 2014 5685", "FO97 5432 0388 8999 44", "FI21 1234 5600 0007 85", "FR14 2004 1010 0505 0001 3M02 606", "GE29 NB00 0000 0101 9049 17", "DE89 3704 0044 0532 0130 00", "GI75 NWBK 0000 0000 7099 453", "GR16 0110 1250 0000 0001 2300 695", "GL56 0444 9876 5432 10", "HU42 1177 3016 1111 1018 0000 0000", "IS14 0159 2600 7654 5510 7303 39", "IE29 AIBK 9311 5212 3456 78", "IL62 0108 0000 0009 9999 999", "IT40 S054 2811 1010 0000 0123 456", "JO94 CBJO 0010 0000 0000 0131 0003 02", "KW81 CBKU 0000 0000 0000 1234 5601 01", "LV80 BANK 0000 4351 9500 1", "LB62 0999 0000 0001 0019 0122 9114", "LI21 0881 0000 2324 013A A", "LT12 1000 0111 0100 1000", "LU28 0019 4006 4475 0000", "MK072 5012 0000 0589 84", "MT84 MALT 0110 0001 2345 MTLC AST0 01S", "MU17 BOMM 0101 1010 3030 0200 000M UR", "MD24 AG00 0225 1000 1310 4168", "MC93 2005 2222 1001 1223 3M44 555", "ME25 5050 0001 2345 6789 51", "NL39 RABO 0300 0652 64", "NO93 8601 1117 947", "PK36 SCBL 0000 0011 2345 6702", "PL60 1020 1026 0000 0422 7020 1111", "PT50 0002 0123 1234 5678 9015 4", "QA58 DOHB 0000 1234 5678 90AB CDEF G", "RO49 AAAA 1B31 0075 9384 0000", "SM86 U032 2509 8000 0000 0270 100", "SA03 8000 0000 6080 1016 7519", "RS35 2600 0560 1001 6113 79", "SK31 1200 0000 1987 4263 7541", "SI56 1910 0000 0123 438", "ES80 2310 0001 1800 0001 2345", "SE35 5000 0000 0549 1000 0003", "CH93 0076 2011 6238 5295 7", "TN59 1000 6035 1835 9847 8831", "TR33 0006 1005 1978 6457 8413 26", "AE07 0331 2345 6789 0123 456", "GB29 NWBK 6016 1331 9268 19"]

wrongIbans = ["GB29 AAAA 6016 1331 9268 19", "AE07 AAAA 2345 6789 0123 456"]

testProperIbans = all (==True) (map iban ibans)
-- quickCheck testProperIbans

testImproperIbans = all (==False) (map iban wrongIbans)
-- quickCheck testImproperIbans
