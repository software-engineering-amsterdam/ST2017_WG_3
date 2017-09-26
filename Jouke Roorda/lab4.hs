import Test.QuickCheck
import Data.List
import SetOrd

toset :: (Ord a) => [a] -> [a]
toset = sort.nub

-- Exercise 2 - 20m --
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

sSymDifference :: Ord a => Set a -> Set a -> Set a
sSymDifference (Set xs) (Set ys) = Set $ toset $ (xs \\\ ys)

