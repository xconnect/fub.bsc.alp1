module Excercise06 where

import Data.Char

-- Aufgabe 2

-- Teil a)
ignoreDoublings :: String -> String
ignoreDoublings [] = []
ignoreDoublings [x] = [x]
ignoreDoublings (x:y:xs)
                | x==y = ignoreDoublings (x:xs)
                | otherwise = x:ignoreDoublings (y:xs)

-- Teil b)
deleteRepetitions :: String -> String
deleteRepetitions [] = []
deleteRepetitions (x:xs) = x:deleteRepetitions [y|y <- xs, y/=x]

-- Teil c)
onlySingles :: String -> String
onlySingles [] = []
onlySingles (x:xs)
            | elem x xs = onlySingles [y | y <- xs, x/=y]
            | otherwise = x:onlySingles xs

-- Teil d)
countSymbols :: String -> [(Char, Int)]
countSymbols [] = []
countSymbols [x] = [(x,1)]
countSymbols (x:xs) = (x,count x (x:xs)):countSymbols [y | y <- xs, y /= x]
 where
  count x [] = 0
  count x (y:ys)
        | x == y = 1 + count x ys
        | otherwise = count x ys

-- Aufgabe 3

-- Teil a)
prodOf2Primes :: Int -> [Int]
prodOf2Primes n = [k | k <- [4..n], a <- [2..n], b <- [2..a], prim a, prim b, a*b == k]
 where
  prim :: Int -> Bool
  prim n
       | n==2 = True
       | otherwise = helper n (n-1)
  helper n k
         | mod n k == 0 = False
         | k==2 && mod n k /= 0 = True
         | mod n k /= 0 = helper n (k-1)

-- Teil b)
squareNearlyInt :: Float -> [Float]
squareNearlyInt a = [x/10 | x <- [0..(10*a)], y <- [0..x*x], (abs (y-((x/10)*(x/10))) <= 0.01)]

-- Teil c)
mirrorCapitals :: String -> String
mirrorCapitals xs = [chr ((90-ord x) + 65) | x <- xs, isUpper x]