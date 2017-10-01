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

symClos' :: Ord a => Rel a -> Rel a
symClos' [] = []
symClos' rs = rl:(swap rl):(symClos $ tail rs) where rl = head rs

symClos :: Ord a => Rel a -> Rel a
symClos = nub.symClos'

-- Exercise 6 - 45m --

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos' ___ [] = []
trClos' orl rs = xs ++ (trClos' orl xs) where xs = rs @@ orl

trClos :: Ord a => Rel a -> Rel a
trClos rs = sort.nub $ rs ++ trClos' rs' rs' where rs' = (filter (not.(uncurry (==))) rs)


-- Exercise 7 - 30m --

-- One of the first things that came to mind for symClos is testing list size,
-- after all, it a list should be twice the size after applying the relation.
-- Unfortunately, it is not. The inverse of a relation could already exist,
-- and a relation with itself does not duplicate. Therefore, it is not a
-- property that is usefull to test.

-- sCElemsTest checks if for every relation, its inverse relation also exists.
sCElemsTest :: Rel Int -> Bool
sCElemsTest xs = all (\ r -> r `elem` sc && (swap r) `elem` sc) xs where sc = symClos xs

-- Unfortunately, it appears to be impossible to create a functioning test for trClos.
-- This is because quickCheck generates looping relations (e.g. (a,b) -> (b,c) -> (c,a)),
-- which cannot be filtered out in trClos without altering the provided @@ operator.
-- This in contrary to self referencing relations (a,a), those are added to the list of
-- possible relations and then removed from the algorithm.
-- To show how looping relations could be handled without getting stuck, tC2' uses
-- filter (not.(flip elem cx)), thus removing already iterated over relations.

-- Should one want to test this without quickCheck, i.e. manually, either tC2 can be used
-- to compare the result to a different implementation of the trClos algorithm, or the
-- solutions should be predefined. One of these manual examples is mentioned in the exercise
-- on blackboard.

tC2' [] o __ = []
tC2' rs o cx = rxs ++ (tC2' rxs o (cx ++ rxs))
    where rxs = filter (not.(flip elem cx)) $ concatMap (\t -> map (\z -> (fst t, snd z)) $ filter (\z -> fst z == (snd t)) o) rs

tC2 rs = rs ++ (tC2' rs' rs' []) where rs' = filter (not.(uncurry (==))) rs

tCTest :: Rel Int -> Bool
tCTest rs = (trClos rs) == (sort.nub $ tC2 rs)
