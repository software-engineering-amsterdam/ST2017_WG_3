module Lab6 where

import Data.List
import Data.Char
import Data.Bits
import System.Random
import Lecture6 hiding (exM, composites, primeMR)

-- #####################################################################################################################
-- Lab Assignment 1
-- Amount of time taken:  2 h
-- #####################################################################################################################

exM :: Integer -> Integer -> Integer -> Integer
exM 0 _ _ = 0
exM _ 0 _ = 1
exM _ _ 0 = error "Remainder cannot be null"
exM num powerNum modNum
             | (powerNum `mod` 2) == 0 = getRemainder
             | otherwise = (num * getRemainder) `mod` modNum
             where recursion = exM num (powerNum `div` 2) modNum
                   getRemainder = recursion^2 `mod` modNum


-- #####################################################################################################################
-- Lab Assignment 2
-- Amount of time taken:  10 min
-- #####################################################################################################################

-- When running:
-- expM 5 5555555555 221
-- It takes more then 30 seconds to complete

-- When running
-- exM 5 5555555555 221
-- It takes less then 1 second to complete

-- This concludes that exM is much more efficient then expM

-- #####################################################################################################################
-- Lab Assignment 3
-- Amount of time taken: 5 minutes
-- #####################################################################################################################

composites :: [Integer]
composites = filter (not.prime) [4..]

-- #####################################################################################################################
-- Lab Assignment 4
-- Amount of time taken:  2h
-- #####################################################################################################################

testF k n = test primeTestsF k 0 (take n composites)

test anonFunc var1 var2 [] = print  "Done"
test anonFunc var1 var2 (naturalNumber:naturalNumbers) =
  do bool  <- anonFunc var1 naturalNumber
     if bool
         then do
              print ("Found false positive on " ++ show naturalNumber)
              test anonFunc var1 (var2+1) naturalNumbers
         else test anonFunc var1 var2 naturalNumbers

-- testF 1 2000
-- k=1 The lowest number I could find is 9
-- k=2 The lowest number I could find is 15
-- k=3 The lowest number I could find is 15
-- k=4 The lowest number I could find is 15
-- k=5 The lowest number I could find is 561
-- k=6 The lowest number I could find is 561
-- k=7 The lowest number I could find is 561
-- k=8 The lowest number I could find is 561
-- k=8 The lowest number I could find is 561

-- When increasing k, it becomes harder and harder to find false positives
-- When k=15 out of the 50 runs I did, only one false positive came back


-- #####################################################################################################################
-- Lab Assignment 5
-- Amount of time taken: 30m
-- #####################################################################################################################

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

carmichaelTest k n = test primeTestsF k 0 (take n carmichael)

-- The test returns a false positive on the first carmichael number: 294409
-- Further on the test is not able to run any longer and returns a memory error

-- #####################################################################################################################
-- Lab Assignment 6
-- Amount of time taken: 30m
-- #####################################################################################################################

testM k n = test primeMR k 0 (take n carmichael)
-- testM 2 100
-- The test takes too long to run

-- #####################################################################################################################
-- Lab Assignment 7 (Exercise 6 was mentioned twice)
-- Amount of time taken: 1h
-- #####################################################################################################################

findMersennes k n = findM k 0 (take n primes)

findM k m [] = print $ show m ++ " Mersenne primes found "
findM k m (n:ns) =
  do bool <- primeMR k (2^n - 1)
     if bool
       then do
         print ("Mersenne prime found for " ++ show n)
         findM k (m+1) ns
       else findM k m ns


-- #####################################################################################################################
-- Lab Assignment 8 RSA
-- Amount of time taken: 2h
-- #####################################################################################################################

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do
    a <- randomRIO (2, n-1) :: IO Integer
    if exM a (n-1) n /= 1 || mrComposite a n
    then return False else primeMR (k-1) n

findPrime :: Int -> IO Integer
findPrime k = do
   p  <- getStdRandom (randomR (2^(k-1), 2^k - 1))
   ok <- primeMR 10 p
   if ok then return p else findPrime k


findPair :: Int -> IO (Integer, Integer)
findPair bits = do
                first <- findPrime bits
                second <- findPrime bits
                if first == second then findPair bits
                else return (first,second)

keyGenerator k = do
                 (first, second) <- findPair k
                 let private = rsaPrivate first second
                 let public = rsaPublic first second
                 return (private, public)

assg8 message = do
                    (privateKey, publicKey) <- keyGenerator 100
                    print $ "Message is: " ++ show message
                    let messageEncoded = rsaEncode publicKey message
                    let messageDecoded = rsaDecode privateKey messageEncoded
                    print $ "Encoded message:" ++ show messageEncoded
                    print $ "Decoded message:" ++ show messageDecoded

-- assg8
-- *Lab6> assg8 123456
-- "Message is: 123456"
-- "Encoded message:274718"
-- "Decoded message:123456"
