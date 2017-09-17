module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

-- #####################################################################################################################
-- Lab Assignment 1
-- Amount of time taken: 1 hour (preparation Haskell IO / Monad)
-- #####################################################################################################################

-- | get all the elements in the list between min max
splitOnMinMax :: [Float] -> Float -> Float -> [Float]
splitOnMinMax list min max = (filter (\n -> n >= min && n < max) list)

checkQuartiles input = do
                    list <- input
                    let first = length (splitOnMinMax list 0.0 0.25)
                    let second = length (splitOnMinMax list 0.25 0.5)
                    let third = length (splitOnMinMax list 0.5 0.75)
                    let fourth = length (splitOnMinMax list 0.75 1.0)
                    return [(1, first), (2,second), (3,third), (4,fourth)]

assignment1 = checkQuartiles (probs 10000)
-- The output:
-- [(1,2383),(2,2639),(3,2491),(4,2487)]
-- [(1,2534),(2,2457),(3,2513),(4,2496)]
-- [(1,2499),(2,2502),(3,2512),(4,2487)]
-- [(1,2544),(2,2534),(3,2436),(4,2486)]
-- [(1,2509),(2,2543),(3,2494),(4,2454)]

-- We tested the amount of numbers in each Quartile.
-- We encounter roughly 2500 in each quartile
--
-- The differences are therefore minimal and this function can be considered random
-- To be sure I ran the test with a larger number of probs, in this case a million (1.000.000)
-- The result is as follows:
-- [(1,250363),(2,249637),(3,249608),(4,250392)]
-- [(1,249816),(2,250341),(3,249867),(4,249976)]
-- [(1,250082),(2,249393),(3,250344),(4,250181)]
-- We can conclude that the differences are much smaller compared to running it with 10.000 elements.
-- This builds a stronger case in proving that this function is random.


-- #####################################################################################################################
-- Lab Assignment 2
-- Recognizing triangles
-- Amount of time taken: 1 hour
-- #####################################################################################################################

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- 1. Haskell Program
triangle :: Integer -> Integer -> Integer -> Shape
triangle x' y' z' = let sorted = sort [x',y',z']
                        x = (head sorted)
                        y = (head (drop 1 sorted))
                        z = (last sorted)
                    in if x + y <= z then NoTriangle
                    else if x == y && y == z then Equilateral
                    else if x^2 + y^2 == z^2 then Rectangular
                    else if x == y || x == z || z == y then Isosceles
                    else Other

-- 2. Concise Test Report
testNoTriangle = (triangle 1 10 5) == NoTriangle
testEquilateral = (triangle 5 5 5) == Equilateral
testRectangular = (triangle 3 4 5) == Rectangular
testIsosceles = (triangle 5 3 5) == Isosceles
testOther = (triangle 4 5 6) == Other

-- Here we test if each If/Else block can be executed if passed the right values.
-- We have proven this is the case since all 5 of the above tests return true

-- We can also test if our let function works as expected:
testTriangleSort = (triangle 1 10 5) == (triangle 10 5 1) && (triangle 10 5 1) == (triangle 10 1 5)

-- #####################################################################################################################
-- Lab Assignment 3
-- Testing properties strength
-- Amount of time taken: 1 hour
-- #####################################################################################################################

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

ass31 = (\ x -> even x && x > 3)
-- Return True since the left part is more specific then even because of the AND operator
ass31Test = stronger [-10..10] ass31 even

ass32 = (\ x -> even x || x > 3)
-- Returns False since an OR statement is used, which makes (even && x>3) stronger then (even)
ass32Test = stronger [-10..10] ass32 even

ass33 = (\ x -> (even x && x > 3) || even x)
-- Returns True since (even == even) so the AND statement can be completely ignored.
-- and (even x && x > 3) is stronger then (even)
ass33Test = stronger [-10..10] ass33 even

ass34 = (\ x -> (even x && x > 3) || even x)
-- Returns True since (even == even) so the AND statement again, can be ignored
ass34Test = stronger [-10..10] even ass34

-- Descending Strength list:
-- ass31 is the strongest
-- ass33 and ass34 are even strong
-- ass32


-- #####################################################################################################################
-- Lab Assignment 4
-- Recognizing Permutations
-- Amount of time taken: 1 hour
-- #####################################################################################################################

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] xs  = False
isPermutation (x:xs) ys = elem x ys && isPermutation xs (delete x ys)

pEqual :: Eq a => [a] -> Bool
pEqual xs = isPermutation xs xs
-- quickCheck pEqual
-- +++ OK, passed 100 tests.

pReversed :: Eq a => [a] -> Bool
pReversed xs = (isPermutation xs (reverse xs))
-- quickCheck pReversed
-- +++ OK, passed 100 tests.

pSorted :: Ord a => [a] -> Bool
pSorted xs = isPermutation xs (sort xs)
-- quickCheck pSorted
-- +++ OK, passed 100 tests.


-- #####################################################################################################################
-- Lab Assignment 5
-- Recognizing and generating derangements
-- Amount of time taken: 1 hour
-- #####################################################################################################################

-- #####################################################################################################################
-- Lab Assignment 6
-- Implementing and testing ROT13 encoding
-- Amount of time taken:
-- #####################################################################################################################

-- #####################################################################################################################
-- Lab Assignment 7
-- Implementing and testing IBAN validation
-- Amount of time taken:
-- #####################################################################################################################

iban :: String -> Bool
iban s = let num = toValidate s in
         if (length s) > 34 then False
         else if num `mod` 97 == 1 then True
         else False

toValidate s = read (concat(map (\n -> if ord n >= 65 && ord n <= 90 then show (convertCharToNum n) else n:[]) ((drop 4 s)++(take 4 s)))) :: Integer

convertCharToNum :: Char -> Int
convertCharToNum s = (ord s) - 55

testListValid = ["AL47212110090000000235698741",
            "AD1200012030200359100100",
            "AT611904300234573201",
            "AZ21NABZ00000000137010001944",
            "BH67BMAG00001299123456",
            "BE62510007547061",
            "BA391290079401028494",
            "BG80BNBG96611020345678",
            "HR1210010051863000160",
            "CY17002001280000001200527600",
            "CZ6508000000192000145399",
            "DK5000400440116243",
            "EE382200221020145685",
            "FO9754320388899944",
            "FI2112345600000785",
            "FR1420041010050500013M02606",
            "GE29NB0000000101904917",
            "DE89370400440532013000",
            "GI75NWBK000000007099453",
            "GR1601101250000000012300695",
            "GL5604449876543210",
            "HU42117730161111101800000000",
            "IS140159260076545510730339",
            "IE29AIBK93115212345678",
            "IL620108000000099999999",
            "IT40S0542811101000000123456",
            "JO94CBJO0010000000000131000302",
            "KW81CBKU0000000000001234560101",
            "LV80BANK0000435195001",
            "LB62099900000001001901229114",
            "LI21088100002324013AA",
            "LT121000011101001000",
            "LU280019400644750000",
            "MK07250120000058984",
            "MT84MALT011000012345MTLCAST001S",
            "MU17BOMM0101101030300200000MUR",
            "MD24AG000225100013104168",
            "MC9320052222100112233M44555",
            "ME25505000012345678951",
            "NL39RABO0300065264",
            "NO9386011117947",
            "PK36SCBL0000001123456702",
            "PL60102010260000042270201111",
            "PT50000201231234567890154",
            "QA58DOHB00001234567890ABCDEFG",
            "RO49AAAA1B31007593840000",
            "SM86U0322509800000000270100",
            "SA0380000000608010167519",
            "RS35260005601001611379",
            "SK3112000000198742637541",
            "SI56191000000123438",
            "ES8023100001180000012345",
            "SE3550000000054910000003",
            "CH9300762011623852957",
            "TN5910006035183598478831",
            "TR330006100519786457841326",
            "AE070331234567890123456",
            "GB29NWBK60161331926819"]

testListInvalid = ["AL472121100900000002535698740",
            "AD12000120302003559100101",
            "AT611904302345753201",
            "AZ21NABZ040000000137010001944",
            "BH67BMAG000055199123456",
            "GB29NWBK601615331926819"]
-- Used a custom generator to do the tests for the credit card
-- Credits to http://blog.nikosbaxevanis.com/2015/02/21/generators-and-the-choose-function/
takeFromList :: [a] -> Gen a
takeFromList xs =
    choose (0, length xs - 1) >>= \i -> return $ xs !! i

testValid = quickCheckResult(forAll (takeFromList testListValid) iban)
testInvalid = quickCheckResult(forAll (takeFromList testListInvalid) (not.iban))


