import Test.QuickCheck
import Data.List
import Data.Tuple
import SetOrd
import Lecture2

-- While this does not convert a list to a Set directly,
-- it does transform it to a set-like list
toset :: (Ord a) => [a] -> [a]
toset = sort.nub

-- Exercise 2 - 30m --
-- More or less does the same as below, but for Int
-- lists only
--instance Arbitrary (Set Int) where
--    arbitrary = do
--        x <- genIntList
--        return $ Set x


instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        x <- arbitrary
        return (Set $ toset $ x) 

-- Check Sets are actually generated and their internal
-- Int lists are sorted and contain no duplicates.
-- Run as quickCheck qcGenSet
qcGenSet :: Set Int -> Bool
qcGenSet (Set xs) = xs == (sort $ nub xs)

infix 1 \\\
(\\\) :: Eq a => [a] -> [a] -> [a]
xs \\\ ys = (filter (flip notElem ys) xs) ++ (filter (flip notElem xs) ys)


-- Exercise 3 - 45m --
sUnion :: Ord a => Set a -> Set a -> Set a
sUnion (Set xs) (Set ys) = Set $ toset $ xs ++ ys

sIntersect :: Ord a => Set a -> Set a -> Set a
sIntersect (Set xs) (Set ys) = Set $ toset $ xs `intersect` ys

sDifference :: Ord a => Set a -> Set a -> Set a
sDifference (Set xs) (Set ys) = Set $ toset $ xs \\ ys

-- Bonus: Symmetric difference
-- sSymDifference :: Ord a => Set a -> Set a -> Set a
-- sSymDifference (Set xs) (Set ys) = Set $ toset $ (xs \\\ ys)

-- The function headers are specifically set to Int,
-- even though this is not necessary for the generator.
-- This is done for readability of the test cases and 
-- to strictly follow the exercise.
testUnion :: [Int] -> [Int] -> Bool
testUnion xs ys = (all (flip elem r) xs) && (all (flip elem r) ys)
    where (Set r) = sUnion (Set xs) (Set ys)

testIntersect :: [Int] -> [Int] -> Bool
testIntersect xs ys = all (\ t -> (t `elem` xs) || (t `elem` ys)) r
    where (Set r) =  sIntersect (Set xs) (Set ys)

testDifference :: [Int] -> [Int] -> Bool
testDifference xs ys = (all (\ t -> (t `elem` xs) && (not $ t `elem` ys)) r)
    where (Set r) = sDifference (Set $ toset xs) (Set $ toset ys)


-- Exercise 5 - 30m --

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos rs = rl:(swap rl):(symClos $ tail rs) where rl = head rs

sCSizeTest :: Rel Int -> Bool
sCSizeTest xs = length xs == (length (symClos xs) `div` 2)

sCElemsTest :: Rel Int -> Bool
sCElemsTest xs = all (\ r -> r `elem` sc && (swap r) `elem` sc) xs where sc = symClos xs

