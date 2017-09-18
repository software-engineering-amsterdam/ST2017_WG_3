
module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- Exercise 1 (1h)
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

---- | logical entailment 
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)
--entails (Impl p q) (Impl (Neg q) (Neg p))

---- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

-- TODO description of your method of checking the definitions


-- Exercise 2
parseTest = do { 
    ; putStrLn (show $ parse "(1==>2)")
    ; putStrLn (show $ parse "+(1 2)")
    ; putStrLn (show $ parse "*(1 2)")
    ; putStrLn (show $ parse "+(-1 -2)")
    ; putStrLn (show $ parse "(1<=>2)")
    ; putStrLn (show $ parse "+((1<=>2) (-3==>4))")}

-- test report describing the test method used and the outcome of the test,


-- Exercise 3
cnf :: Form -> Form 
cnf x = cnf' $ nnf $ arrowfree x

cnf' :: Form -> Form 
cnf' (Prop x) = Prop x
cnf' (Neg (Prop x)) = (Neg (Prop x))
cnf' (Neg (Neg x)) = cnf' x
cnf' (Neg (Cnj xs)) = cnf' (Dsj (map (cnf'.Neg) xs)) -- Only props may have Negations
cnf' (Neg (Dsj xs)) = cnf' (Cnj (map (cnf'.Neg) xs)) -- Only props may have Negations
cnf' (Dsj [x, (Dsj [y, z])]) = Cnj [(Dsj [(cnf' x), (cnf' y)]), (Dsj [(cnf' x), (cnf' z)])]
--cnf' (Dsj [(Dsj [y, z]), x]) = Cnj [(Dsj [(cnf x), (cnf y)]), (Dsj [(cnf x), (cnf z)])]
cnf' (Dsj [(Dsj [y, z]), x]) = cnf' (Dsj [x, (Dsj [y, z])])
cnf' (Dsj [x, y]) = Dsj [(cnf' x), (cnf' y)]
cnf' (Cnj [x, y]) = Cnj [(cnf' x), (cnf' y)]


-- Exercise 4
--equiv (cnf $ Dsj [Cnj [p, q], r]) (Dsj [Cnj [p, q], r])

-- equiv form1 (cnf $ nnf $ arrowfree form1)

myForm = (Impl (Dsj [p, q]) p)
