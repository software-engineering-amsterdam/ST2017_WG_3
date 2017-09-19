import Data.List
import Data.Char
import Data.Fixed
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Exercise 1 - 2h (mainly for test/avgDeviation) --

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
        p <- getStdRandom random
        ps <- probs (n-1)
        return (p:ps)

-- https://stackoverflow.com/a/26372259
sofrequency :: Ord a => [a] -> [Int]
sofrequency = map length.group.sort

-- dist :: Int -> IO [Int]
dist n = do
        ps <- probs n
        return $ sofrequency $ map (\ n -> n - (mod' n 0.25)) ps

avgDeviation' :: Int -> IO [Int]
avgDeviation' 0 = return []
avgDeviation' i = do
        dst <- dist 100000
        dstx <- avgDeviation' (i-1)
        return (((flip div 4).sum $ map (abs.(subtract 25000)) dst):dstx)

-- Avg deviation over 100 runs
avgDeviation :: IO Int
avgDeviation = do
        r <- avgDeviation' 100
        return $ (sum r) `div` (length r)

-- While not giving a perfect 1/4 per quarter distribution,
-- the avgDeviation function observes deviations around 100-115
-- on a 25000 numbers per quarter basis. This equals percentages
-- of 0.40-0.46%
probsExample = dist 100000

-- A test would be to check if the diviation stays within 0.05%
-- This would look like the following:
probsTest = do
        r <- avgDeviation
        return $ r <= 125

-- Exercise 2 - 30m --

data Shape = NoTriangle | Equilateral 
        | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
        | a <= 0 || b <= 0 || c <= 0 = NoTriangle
        | a == b && b == c           = Equilateral
        | a == b || b == c || a == c = Isosceles
        | a^2 + b^2 == c^2           = Rectangular
        | a^2 + c^2 == b^2           = Rectangular
        | b^2 + c^2 == a^2           = Rectangular
        | otherwise                  = Other

-- Exercise 3 - 1h --

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

p1, p2, p3, p4 :: Int -> Bool
p1 = (\ x -> even x && x > 3)
p2 = (\ x -> even x || x > 3)
p3 = (\ x -> (even x && x > 3) || even x)
p4 = even

properties = [("p1", p1), ("p2", p2), ("p3", p3), ("p4", p4)]

getName (n, _) = n
getFunc (_, f) = f

quicksort' :: [a] -> [(t, a -> Bool)] -> [(t, a -> Bool)]
quicksort' _ [] = []  
quicksort' r (x:xs) = 
   quicksort' r [ a | a <- xs, stronger r (getFunc x) (getFunc a)]
   ++ [x]
   ++ quicksort' r [ a | a <- xs, not $ stronger r (getFunc x) (getFunc a)]

-- When using stronger and weaker, p4 will occur twice as it is as strong as p3,
-- given that the "&& x > 3" is useless with the "|| even x" in place

lstxx :: [[Char]]
lstxx = map getName $ quicksort' [-10..10] properties

-- Exercise 4 - 1h --
-- https://stackoverflow.com/a/29307068
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

teq :: Eq a => (a, a) -> Bool
teq = uncurry (==)

infix 1 ^^^
(^^^) :: (a -> b) -> (a, a) -> (b, b)
f ^^^ (y, z) = (f y, f z)

isPermutation' :: Eq a => ([a], [a]) -> [Bool]
isPermutation' ([], []) = [True]
isPermutation' (xs, []) = [False]
isPermutation' ([], ys) = [False]
isPermutation' (xs, ys) = (teq $ count (head xs) ^^^ (xs, ys)):(isPermutation' $ (filter (/= head xs)) ^^^ (xs, ys))

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = all (==True) $ isPermutation' (xs, ys)


-- Exercise 5 - 1h --

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y = (isPermutation x y) && (not $ any teq $ zip x y)

deran :: (Enum a, Eq a, Num a) => a -> [[a]]
deran n = filter (isDerangement [0..n-1]) $ permutations [0..n-1]

testIsDerangementTrue = all (uncurry isDerangement) [([1,2,3,4], [4,3,2,1]), ([1,2,3,4], [2,3,4,1])]
testIsDerangementFalse = all (not.(uncurry isDerangement)) [([1,2,3,4], [5,6,7,8]), ([1,2,3,4], [1,2,3,4]), ([1,1,1,1], [1,1,1,1])]

testIsDerangement :: Bool
testIsDerangement = testIsDerangementTrue && testIsDerangementFalse

-- Exercise 6 --

selectBase x | x `elem` [65..90] = 65
selectBase x | x `elem` [97..122] = 97
selectBase x | otherwise = 0

selectMod x | x `elem` [65..90] = 90
selectMod x | x `elem` [97..122] = 122
selectMod x | otherwise = 1024

rot13'' :: Char -> Char
rot13'' x = chr.(+ z).(flip mod 25).(+ 13).(- z) $ y
        where
          y = ord x
          z = selectBase y

rot13' [] = ""
rot13' (x:xs) = (rot13'' x):(rot13' xs)

-- Exercise 7 - 30m --

convertAscii :: Integral a => a -> a
convertAscii x | x `elem` [65..90] = x `mod` 55
convertAscii x | x `elem` [48..57] = x `mod` 48
convertAscii x | otherwise         = x

iban' :: [Char] -> Integer
iban' s = read (foldr (++) "" $ map (show.convertAscii.ord) s) :: Integer

iban :: String -> Bool
iban n = (==1) $ (flip mod 97) $ iban' $ (drop 4 n) ++ take 4 n

testIban :: Bool
testIban = all (==True) $ map iban ibans

-- Royal Bank of Scotland's website - ironically - has their own IBAN incorrectly formatted
-- in the list of examples.
-- Its RBOS part is supposed to be NWBK. This has been changed in the list below.
ibans = ["AL47212110090000000235698741","AD1200012030200359100100","AT611904300234573201","AZ21NABZ00000000137010001944","BH67BMAG00001299123456","BE62510007547061","BA391290079401028494","BG80BNBG96611020345678","HR1210010051863000160","CY17002001280000001200527600","CZ6508000000192000145399","DK5000400440116243","EE382200221020145685","FO9754320388899944","FI2112345600000785","FR1420041010050500013M02606","GE29NB0000000101904917","DE89370400440532013000","GI75NWBK000000007099453","GR1601101250000000012300695","GL5604449876543210","HU42117730161111101800000000","IS140159260076545510730339","IE29AIBK93115212345678","IL620108000000099999999","IT40S0542811101000000123456","JO94CBJO0010000000000131000302","KW81CBKU0000000000001234560101","LV80BANK0000435195001","LB62099900000001001901229114","LI21088100002324013AA","LT121000011101001000","LU280019400644750000","MK07250120000058984","MT84MALT011000012345MTLCAST001S","MU17BOMM0101101030300200000MUR","MD24AG000225100013104168","MC9320052222100112233M44555","ME25505000012345678951","NL39RABO0300065264","NO9386011117947","PK36SCBL0000001123456702","PL60102010260000042270201111","PT50000201231234567890154","QA58DOHB00001234567890ABCDEFG","RO49AAAA1B31007593840000","SM86U0322509800000000270100","SA0380000000608010167519","RS35260005601001611379","SK3112000000198742637541","SI56191000000123438","ES8023100001180000012345","SE3550000000054910000003","CH9300762011623852957","TN5910006035183598478831","TR330006100519786457841326","AE070331234567890123456","GB29NWBK60161331926819"]

