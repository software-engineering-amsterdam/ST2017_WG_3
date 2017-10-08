-- Exercise 3 (3h)

module Lecture5Sol3

where 

import Data.List
import System.Random
import Lecture5

-- Tried this but not sure why it doesn't work?!
-- Though 100 tests would be kinda slow. 
--randMinimalTest :: IO Bool
--randMinimalTest s = do { ; p <- genProblem s
--                         ; return (uniqueSol p && all (\x -> len

-- instance Arbitrary (Form) where
--    arbitrary = genRandomSudoku

-- > quickCheck randMinimalTest

removeHint :: Node -> [Node]
removeHint x = [eraseN x n | n <- (filledPositions (fst x))]

randMinimalTest :: IO Bool
randMinimalTest = do { ; sud <- genRandomSudoku
                       ; prob <- genProblem sud
                       ; return (uniqueSol prob && all (\x -> length (take 2 (solveNs [x])) == 2) (removeHint prob)) } 

testMinimalNTimes :: (Eq a, Num a) => a -> IO ()
testMinimalNTimes 0 = return ()
testMinimalNTimes n = do { ; t <- randMinimalTest
                           ; if t then putStrLn "Success" else putStrLn "Failed!"
                           ; testMinimalNTimes (n-1) }

-- > testMinimalNTimes 10
