module Ex4 where

import Data.List
import System.Random
import Lecture5
import Data.Tuple


-- #####################################################################################################################
-- Lab Assignment 4
-- Amount of time taken: 1 hours
-- #####################################################################################################################

getRandomBlockPositions :: IO [(Int, Int)]
getRandomBlockPositions = do

                            i <- randomRIO (0, 2)
                            i2 <- randomRIO (0, 2)
                            return [
                             (head $ (blocks !! i), head $ (blocks !! i2)),
                             (head (blocks !! i), head (blocks !! i2) + 1),
                             (head (blocks !! i), head (blocks !! i2) + 2),
                             (head (blocks !! i) + 1, head (blocks !! i2)),
                             (head (blocks !! i) + 1, head (blocks !! i2) + 1),
                             (head (blocks !! i) + 1, head (blocks !! i2) + 2),
                             (head (blocks !! i) + 2, head (blocks !! i2)),
                             (head (blocks !! i) + 2, head (blocks !! i2) + 1),
                             (head (blocks !! i) + 2, head (blocks !! i2) + 2)
                             ]

genProblemBlock :: Node -> IO Node
genProblemBlock n = do ys <- randomize xs
                       pos1 <- getRandomBlockPositions
                       pos2 <- getRandomBlockPositions
                       pos3 <- getRandomBlockPositions
                       pos4 <- getRandomBlockPositions
                       return (minimalize (emptyPositions n (pos1 ++ pos2 ++ pos3 ++ pos4)) ys)
                where xs = filledPositions (fst n)

emptyPositions :: Node -> [(Row,Column)] -> Node
emptyPositions n [] = n
emptyPositions n (h:t) = emptyPositions (eraseN n h) t

main :: IO ()

main = do r <- genRandomSudoku
          putStr "\nOriginal Solution\n"
          showNode r
          s  <- genProblemBlock r
          putStr "\nProblem\n"
          showNode s
          putStr "\nSolved\n"
          showNode $ head $ solveNs [s]



-- Output:
-- Original Solution
-- +-------+-------+-------+
-- | 9 8 6 | 5 7 2 | 1 3 4 |
-- | 4 5 1 | 9 3 6 | 7 8 2 |
-- | 7 2 3 | 1 8 4 | 6 5 9 |
-- +-------+-------+-------+
-- | 2 9 5 | 4 6 3 | 8 7 1 |
-- | 3 6 8 | 7 1 9 | 4 2 5 |
-- | 1 4 7 | 8 2 5 | 3 9 6 |
-- +-------+-------+-------+
-- | 6 7 9 | 2 4 8 | 5 1 3 |
-- | 5 1 4 | 3 9 7 | 2 6 8 |
-- | 8 3 2 | 6 5 1 | 9 4 7 |
-- +-------+-------+-------+
--
-- Problem
-- +-------+-------+-------+
-- | 9   6 | 5 7   | 1   4 |
-- |   5   | 9     | 7     |
-- |     3 |     4 | 6     |
-- +-------+-------+-------+
-- |       |     3 |       |
-- |       |   1   |       |
-- |       | 8 2 5 |       |
-- +-------+-------+-------+
-- | 6   9 |       | 5     |
-- |     4 |       | 2 6 8 |
-- |     2 |       |     7 |
-- +-------+-------+-------+
--
-- Solved
-- +-------+-------+-------+
-- | 9 8 6 | 5 7 2 | 1 3 4 |
-- | 4 5 1 | 9 3 6 | 7 8 2 |
-- | 7 2 3 | 1 8 4 | 6 5 9 |
-- +-------+-------+-------+
-- | 2 9 5 | 4 6 3 | 8 7 1 |
-- | 3 6 8 | 7 1 9 | 4 2 5 |
-- | 1 4 7 | 8 2 5 | 3 9 6 |
-- +-------+-------+-------+
-- | 6 7 9 | 2 4 8 | 5 1 3 |
-- | 5 1 4 | 3 9 7 | 2 6 8 |
-- | 8 3 2 | 6 5 1 | 9 4 7 |
-- +-------+-------+-------+
