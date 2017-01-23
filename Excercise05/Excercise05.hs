module Excercise05 where

-- Aufgabe 1)

-- Teil a)

-- rekursive Berechnung von superFib
superFib :: Int -> Int
superFib n
         | n==0 || n==1 = 0
         | n==2         = 1
         | otherwise    = superFib (n-1) + superFib (n-2) + superFib (n-3)

-- Teil b)

-- zählt die Funktionsaufrufe von superFib
countCalls :: Int -> Int
countCalls n
           | n==0 || n==1 || n==2 = 1
           | otherwise = 1 + countCalls (n-1) + countCalls (n-2) + countCalls (n-3)

-- einfach weil es geht...
superFibWithCountCalls :: Int -> (Int, Int)
superFibWithCountCalls n = (superFib n, countCalls n)
   
-- Aufgabe 2)

type Zeit = (Int, Int)

-- Teil a)
addStd :: Zeit -> Int -> Zeit
addStd (h,m) hours = (mod (h+hours) 24,m)

addMin :: Zeit -> Int -> Zeit
addMin (h,m) minutes = addStd (h,mod (m+minutes) 60) (div (m+minutes) 60)

-- Teil b)
dauer :: Zeit -> Zeit -> Zeit
dauer (a,b) (c,d)
      | minuten a b >= minuten c d = (div (minuten a b) 60, mod (minuten c d) 60)
      | otherwise                  = (div (minuten c d) 60, mod (minuten a b) 60)
 where
  minuten x y = x*60 + y