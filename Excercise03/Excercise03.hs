module Excercise03 where

-- Aufgabe 1)

-- Teil a)

-- teste, ob a+b=c oder b+c=a oder c+a=b
sumInclude3 :: Int -> Int -> Int -> Bool
sumInclude3 a b c = a+b==c || b+c==a || a+c==b

-- Teil b)

-- Kommentar hier
sumInclude4 :: Int -> Int -> Int -> Int -> Bool
sumInclude4 a b c d = a+b == c || a+b == d
                   || a+c == b || a+c == d
                   || a+d == b || a+d == c
                   || b+c == a || b+c == d
                   || b+d == a || b+d == c
                   || c+d == a || c+d == b

-- Teil c)

-- Kommentar hier
spanOf3 :: Float -> Float -> Float -> Float
spanOf3 a b c = maksimum a b c - mienimum a b c
   where
      maksimum :: Float -> Float -> Float -> Float
      maksimum a b c
               | a<=b && b<=c = c
               | c<=a && a<=b = b
               | b<=c && c<=a = a
      mienimum :: Float -> Float -> Float -> Float
      mienimum a b c
               | a<=b && b<=c = a
               | c<=a && a<=b = c
               | b<=c && c<=a = b

-- Teil d)

-- Kommentar hier
thirdMan :: Int -> Int -> Int
thirdMan a b = 6 - (a+b)

-- Aufgabe 2)

-- Teil a)

-- Kommentar hier
valueAt :: Float -> Float -> Float -> Float
valueAt xg yg x = (-yg/xg)*x+yg

-- Teil b)

-- Kommentar hier
testParallel :: Float -> Float -> Float -> Float -> Bool
testParallel xg yg xh yh = mg == mh
   where
      mg = -yg/xg
      mh = -yh/xh
  
-- Teil c)

-- Kommentar hier
parallelThroughX :: Float -> Float -> Float -> Float
parallelThroughX xg yg x = yg/xg*x

-- Teil d)

-- Kommentar hier
crossingAt :: Float -> Float -> Float -> Float -> Float
crossingAt xg yg xh yh = (yg-yh)/(mh-mg)
   where
      mg = -yg/xg
      mh = -yh/xh

-- Teil e)

-- Kommentar hier
computeXg :: Float -> Float -> Float -> Float
computeXg a b c
          | a == 0 || b == 0 || c == 0 = error "Fehler!"
          | otherwise = c/a

-- Kommentar hier
computeYg :: Float -> Float -> Float -> Float		  
computeYg a b c
          | a == 0 || b == 0 || c == 0 = error "Fehler!"
          | otherwise = c/b

-- Aufgabe 3)

-- tested, ob n eine Primzahl ist
primzahlTest :: Int -> Bool
primzahlTest n = null [x | x <- [2..(n-1)], (mod n x)==0]