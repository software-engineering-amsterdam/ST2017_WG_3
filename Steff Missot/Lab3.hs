module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- #####################################################################################################################
-- Lab Assignment 1
-- Amount of time taken: 1.5 hours
-- #####################################################################################################################

contradiction :: Form -> Bool
contradiction a = not (satisfiable a)
-- Test:
-- contradiction (Equiv (Neg p) p) == True
-- contradiction (Equiv p p) == False

tautology :: Form -> Bool
tautology a = all (\ v -> evl v a) (allVals a)
-- Test:
-- tautology (Dsj [p, Neg p]) == True
-- tautology (Dsj [p, p]) == False
-- tautology (Dsj [Neg p, Neg p]) == False
-- tautology (Equiv (Impl p q) (Impl (Neg q) (Neg p))) == True

-- | logical entailment
entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)
-- Test
-- entails (Impl p q) (Impl (Neg q) (Neg p)) == True
-- p -> q === ~q -> p

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b)


-- I have checked the definitions by providing examples from truth tables. to see the outcome

-- #####################################################################################################################
-- Lab Assignment 2
-- Amount of time taken: 2 hours
-- #####################################################################################################################

-- The random Generator is taken from https://homepages.cwi.nl/~jve/courses/14/fsa/lectures/FSA6.pdf
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomForm :: Int -> IO Form
getRandomForm 0 = do
                    m <- getRandomInt 20
                    return (Prop (m+1))
getRandomForm d = do
                    n <- getRandomInt 5
                    case n of
                        0 -> do m <- getRandomInt 20
                                return (Prop (m+1))
                        1 -> do f <- getRandomForm (d-1)
                                return (Neg f)
                        2 -> do m <- getRandomInt 5
                                fs <- getRandomForms (d-1) m
                                return (Cnj fs)
                        3 -> do m <- getRandomInt 5
                                fs <- getRandomForms (d-1) m
                                return (Dsj fs)
                        4 -> do f <- getRandomForm (d-1)
                                g <- getRandomForm (d-1)
                                return (Impl f g)
                        5 -> do f <- getRandomForm (d-1)
                                g <- getRandomForm (d-1)
                                return (Equiv f g)

getRandomForms :: Int -> Int -> IO [Form]
getRandomForms _ 0 = return []
getRandomForms d n = do
            f <- getRandomForm d
            fs <- getRandomForms d (n-1)
            return (f:fs)


getRandomFs :: Int -> IO [Form]
getRandomFs n = do
            d <- getRandomInt 3
            s <- getRandomForms d n
            return s

test :: Int -> (Form -> Bool) -> [Form] -> IO ()
test n _ [] = print (show n ++ " tests passed")
test n p (f:fs) = if p f then do
                    print ("pass on:" ++ show f)
                    test n p fs
                  else
                    error ("failed test on:" ++ show f)

testForms :: Int -> (Form -> Bool) -> IO ()
testForms n p = do
    fs <- getRandomFs n
    test n p fs

testParser = testForms 100
    (\ f -> let [g] = parse (show f) in f == g)

-- The testParser function checks if the string that is put in before calling the parse method
-- is present in the string that the parse method returns.

-- #####################################################################################################################
-- Lab Assignment 3
-- Amount of time taken: 3 hours
-- #####################################################################################################################

convertToCnf :: Form -> Form
convertToCnf a = cnf $ nnf $ arrowfree a

cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Dsj [Cnj a, b]) = Cnj [Dsj [(head a), cnf b], Dsj [(last a), cnf b]]
cnf (Dsj [a, Cnj b]) = cnf (Dsj [Cnj b, a])
cnf (Cnj a) = Cnj (map cnf a)
cnf (Dsj a) = Dsj (map cnf a)


-- Test
-- equiv (convertToCnf $ Dsj [r, Cnj [p, q]]) (Dsj [r, Cnj [p, q]])

-- The below check is used to check fto check if the Distributive Law is applied correctly
-- show (convertToCnf $ Dsj [r, Cnj [p, q]]) == show (Cnj [Dsj [p,r], Dsj [q,r]])

-- equiv (convertToCnf $ Dsj [Cnj [r, q], Cnj [p, q]]) (Dsj [Cnj [r,q], Cnj [p, q]])

-- #####################################################################################################################
-- Lab Assignment 4
-- Amount of time taken: X hours
-- #####################################################################################################################