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

testParser :: Form -> Bool
testParser x = (== x) $ head $ parse $ show x

testParser1 = testParser (Neg $ Equiv (Prop 1) (Prop 0))
testParser2 = testParser (Neg $ Equiv (Prop 1) (Prop 1))

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

flatten :: Form -> Form
flatten (Cnj [Cnj x, Prop y]) = Cnj $ (Prop y):(map flatten x)
flatten (Cnj [Prop y, Cnj x]) = flatten $ Cnj [Cnj x, Prop y]
flatten x = x

cnf :: Form -> Form
cnf = flatten.distr.nnf.arrowfree


