-- 19:30 - 21:40

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree

-- Inspired by https://math.stackexchange.com/questions/214338/how-to-convert-to-conjunctive-normal-form
-- applyance of first and second De Morgan's laws and Distributive laws
cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg (Dsj [a, b])) = Cnj [Neg (cnf' a), Neg (cnf' b)] -- De Morgan's 1
cnf' (Neg (Cnj [a, b])) = Dsj [Neg (cnf' a), Neg (cnf' b)] -- De Morgan's 2
cnf' (Dsj [a, Cnj [b, c]]) = Cnj [Dsj [cnf' a, cnf' b], Dsj [cnf' a, cnf' c]] -- Distributive 1
cnf' (Cnj [a, Dsj [b, c]]) = Dsj [Cnj [cnf' a, cnf' b], Cnj [cnf' a, cnf' c]] -- Distributive 2
