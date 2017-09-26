import Test.QuickCheck
import Data.List
import SetOrd
import Lecture2

toset :: (Ord a) => [a] -> [a]
toset = sort.nub

-- Exercise 2 - 20m --

--instance Arbitrary (Set Int) where
--    arbitrary = do
--        x <- genIntList
--        return $ Set x


instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        x <- arbitrary
        return (Set $ toset $ x) 

qcGenSet :: Set Int -> Bool
qcGenSet (Set xs) = xs == (sort $ nub xs)

infix 1 \\\
(\\\) :: Eq a => [a] -> [a] -> [a]
xs \\\ ys = (filter (flip notElem ys) xs) ++ (filter (flip notElem xs) ys)


-- Exercise 3 - 20m --
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

