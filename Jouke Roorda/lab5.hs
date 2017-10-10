
module Lab5

where

import Data.List
import Data.List.Split
import Data.Tuple
import Lecture5

ex1sud :: [[Value]]
ex1sud = [
    [0,0,0,3,0,0,0,0,0],
    [0,0,0,7,0,0,3,0,0],
    [2,0,0,0,0,0,0,0,8],
    [0,0,6,0,0,5,0,0,0],
    [0,9,1,6,0,0,0,0,0],
    [3,0,0,0,7,1,2,0,0],
    [0,0,0,0,0,0,0,3,1],
    [0,8,0,0,4,0,0,0,0],
    [0,0,2,0,0,0,0,0,0]]

validSud :: [[Value]]
validSud = [
    [4,7,8,3,9,2,6,1,5],
    [6,1,9,7,5,8,3,2,4],
    [2,3,5,4,1,6,9,7,8],
    [7,2,6,8,3,5,1,4,9],
    [8,9,1,6,2,4,7,5,3],
    [3,5,4,9,7,1,2,8,6],
    [5,6,7,2,8,9,4,3,1],
    [9,8,3,1,4,7,5,6,2],
    [1,4,2,5,6,3,8,9,7]]

printHBorder = do putStrLn "+---------+---------+---------+"
printInterBorder = do putStrLn "|   +-----|--+   +--|-----+   |"

showExtRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] =
 do putChar '|'         ;
    putChar ' '; putStr (showVal a1) ; putChar ' '
    putChar ' '; putStr (showVal a2) ; putChar ' '
    putChar ' '; putStr (showVal a3) ; putChar ' '
    putChar '|'         ;
    putChar ' '; putStr (showVal a4) ; putChar ' '
    putChar ' '; putStr (showVal a5) ; putChar ' '
    putChar ' '; putStr (showVal a6) ; putChar ' '
    putChar '|'         ;
    putChar ' '; putStr (showVal a7) ; putChar ' '
    putChar ' '; putStr (showVal a8) ; putChar ' '
    putChar ' '; putStr (showVal a9) ; putChar ' '
    putChar '|'         ; putChar '\n'
    
showExtHRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] =
 do putChar '|'         ;
    putChar ' '; putStr (showVal a1) ; putChar ' '
    putChar '|'; putStr (showVal a2) ; putChar ' '
    putChar ' '; putStr (showVal a3) ; putChar ' '
    putChar '|'         ;
    putChar ' '; putStr (showVal a4) ; putChar '|'
    putChar ' '; putStr (showVal a5) ; putChar ' '
    putChar '|'; putStr (showVal a6) ; putChar ' '
    putChar '|'         ;    
    putChar ' '; putStr (showVal a7) ; putChar ' '
    putChar ' '; putStr (showVal a8) ; putChar '|'
    putChar ' '; putStr (showVal a9) ; putChar ' '
    putChar '|'         ; putChar '\n'

showExtSud [as,bs,cs,ds,es,fs,gs,hs,is] =
 do printHBorder
    showExtRow as
    printInterBorder
    showExtHRow bs
    showExtHRow cs
    printHBorder
    showExtHRow ds
    printInterBorder
    showExtRow es
    printInterBorder
    showExtHRow fs
    printHBorder
    showExtHRow gs
    showExtHRow hs
    printInterBorder
    showExtRow is
    printHBorder

ebl x = concat $ filter (elem x) [[2..4],[6..8]]

none f xs = not $ any f xs
gridComplete g = none (any (== 0)) g

coordRow    (r,c) = [(r,x) | x <- [1..9]] ++ [(x,c) | x <- [1..9]]
coordNBlock (r,c) = [(x,y) | x <- bl r, y <- bl c]
coordEBlock (r,c) = [(x,y) | x <- ebl r, y <- ebl c]

usedInRow g (r,c) = g !! r
usedInCol g (r,c) = transpose g !! c
usedInRows g (r,c) = (g !! r) ++ (transpose g !! c)
usedInNBlock g (r,c) = nub $ map (\t -> g !! (fst t -1) !! (snd t -1)) $ coordNBlock (r+1,c+1)
usedInEBlock g (r,c) = nub $ map (\t -> g !! (fst t -1) !! (snd t -1)) $ coordEBlock (r+1,c+1)


usedNums g (r,c) = usedInRows g (r,c) ++ (usedInNBlock g (r,c)) ++ (usedInEBlock g (r,c))

availableNums g (r,c) | g !! r !! c /= 0 = [g !! r !! c]
availableNums g (r,c) | otherwise        = [1..9] \\ (usedNums g (r,c))

possibleNums g (r,c) = availableNums g (r,c)

pN1 g (r,c) = (\\) [1..9] $ concatMap (\t -> possibleNums g (fst t -1, snd t -1))
              $ (coordRow (r+1,c+1) \\ [(r+1,c+1)])
pN2 g (r,c) = (\\) [1..9] $ concatMap (\t -> possibleNums g (fst t -1, snd t -1))
              $ (coordNBlock (r+1,c+1) \\ [(r+1,c+1)])
pN3 g (r,c) = (\\) [1..9] $ concatMap (\t -> possibleNums g (fst t -1, snd t -1))
              $ (coordEBlock (r+1,c+1) \\ [(r+1,c+1)])

pNx g c = sort.nub $ pN1 g c ++ (pN2 g c) ++ (pN3 g c)

newValue g (r,c) av | g !! r !! c /= 0 = g !! r !! c
newValue g (r,c) av | length av == 1   = head av
newValue _ _____ __ | otherwise        = 0

rep' [] _ _ _ = []
rep' (x:xs) i v z | i == z = v:xs
rep' xs i v z | otherwise = rep' xs (i+1) v z

rep l i v = rep' l i v 0

validBlock b = (==[1..9]) $ sort $ nub b

validExtSud g = (all
    (\c -> (validBlock $ usedInRow g c)
        && (validBlock $ usedInCol g c)
        && (validBlock $ usedInNBlock g c))
            [(a,b) | a <- [0..8], b <- [0..8]])
    && (all 
        (\c -> (validBlock $ usedInEBlock g c))
            [(x,y) | x <- ([1..3] ++ [5..7]), y <- ([1..3] ++ [5..7])])

bruteforce g = g -- head $ dropWhile (not.validExtSud) $ chunksOf 9 $ chunksOf 9 $ [p | p <- [pNx g (r,c) | r <- [0..8], c <- [0..8]]]

solveWhile g True = g
solveWhile g ____ = if g == g'
                    then bruteforce g
                    else solveWhile g' (gridComplete g')
    where g' = map (\r -> map (\c -> newValue g (r, c) (pNx g (r, c))) [0..8]) [0..8]









