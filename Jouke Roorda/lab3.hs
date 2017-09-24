import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise 1 - 2h --

contradiction :: Form -> Bool
contradiction = not.satisfiable

tautology :: Form -> Bool
tautology f = all (flip evl f) $ allVals f

cmpLists :: (Bool -> Bool -> Bool) -> ([Bool], [Bool]) -> [Bool]
cmpLists f ([]    , []    ) = [True]
cmpLists f ([]    , __    ) = [False]
cmpLists f (__    , []    ) = [False]
cmpLists f ((x:xs), (y:ys)) = (f x y):(cmpLists f (xs, ys))

evl2fs :: Form -> Form -> ([Bool], [Bool])
evl2fs f1 f2 = (map (flip evl f1) (allVals f1), map (flip evl f2) (allVals f2))

-- | logical entailment 
entails :: Form -> Form -> Bool
entails f1 f2 = (all (== True)).(cmpLists (-->)) $ evl2fs f1 f2

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = (all (== True)).(cmpLists (==)) $ evl2fs f1 f2


contradictionTest = contradiction (Equiv (Prop 0) (Neg $ Prop 0)) == True && (not $ contradiction (Equiv (Prop 0) (Prop 0)))
tautologyTest = tautology (Equiv (Prop 0) (Prop 0)) && (not $ tautology (Equiv (Prop 0) (Neg $ Prop 0)))
entailsTest = entails (Equiv (Prop 0) (Neg $ Prop 0)) (Prop 1) && (entails (Prop 0) (Prop 0)) && (not $ entails (Prop 0) (Equiv (Prop 1) (Neg $ Prop 1)))
equivTest = equiv (Prop 1) (Prop 1) && (not $ equiv (Prop 0) (Neg $ Prop 1))

-- Exercise 2 - 10m --

testParser' :: Form -> Bool
testParser' x = (== x).head.parse $ show x

-- Test a variety of logic constructions
testParser :: Bool
testParser = all testParser' [x | let q = [Prop i | i <- [1..5]],
                                  z <- [Cnj q, Dsj q, Prop 0],
                                  y <- [Impl z z, Equiv z z],
                                  x <- [y, Neg y]]

-- In addition to this, given the generator written for exercise 4,
-- just "quickCheck testParser'" now works too!

qcTestParser = quickCheck testParser'

-- Exercise 3 - 4h --
isCnj :: [Form] -> Bool
isCnj [Cnj _, Cnj _] = True
isCnj [Cnj _, Prop _] = True
isCnj [Prop _, Cnj _] = True
isCnj _ = False

distr'' :: [Form] -> [Form]
distr'' [Cnj t, Cnj f] = [Dsj [a, b] | a <- t, b <- f]
distr'' [Prop p, Cnj f] = [Dsj [Prop p, a] | a <- f]
distr'' [Cnj f, Prop p] = distr'' [Prop p, Cnj f]

distrd :: [Form] -> Form
distrd xs | isCnj xs  = Cnj $ distr'' xs
distrd xs | otherwise = Dsj xs

distr :: Form -> Form
distr (Dsj t) = distrd t
distr (Cnj t) = Cnj $ map distr t
distr (Prop p) = Prop p
distr x = x

flatten :: Form -> Form
flatten (Cnj [Cnj x, Prop y]) = Cnj $ (Prop y):(map flatten x)
flatten (Cnj [Prop y, Cnj x]) = flatten $ Cnj [Cnj x, Prop y]
flatten x = x

cnf :: Form -> Form
cnf = flatten.distr.nnf.arrowfree

-- Exercise 4 - 2h --

instance Arbitrary (Form) where
    arbitrary = sized $ sizedArbitraryForm

sizedArbitraryForm :: Int -> Gen Form
sizedArbitraryForm 0 = do
        n <- choose (0, 100)
        return $ Prop n

sizedArbitraryForm n = do
        n <- choose (0, n)
        r <- choose (0, 100)
        -- divide by two to prevent gigantic formulas from being generated
        ts1 <- sizedArbitraryForm (n `div` 2)
        ts2 <- sizedArbitraryForm (n `div` 2)
        t <- elements [Prop r, Neg ts1, Cnj [ts1, ts2], Dsj [ts1, ts2], Impl ts1 ts2, Equiv ts1 ts2]
        return t

cnfTest :: Form -> Bool
cnfTest f = equiv f (cnf f)

cnfNTest :: Form -> Bool
cnfNTest f = not $ equiv f (Neg $ cnf f)