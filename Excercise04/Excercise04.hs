module Excercise04 where


-- Aufgabe 1: Polynome

-- Teil a)
eval3 :: Float -> Float -> Float -> Float -> Float -> Float
eval3 r a3 a2 a1 a0
      | a3 /= 0 = a3*r**3 + a2*r**2 + a1*r + a0
      | otherwise = error "Kein Polynom vom Grad 3!"

-- Teil b)
nullstellentest2 :: Float -> Float -> Float -> Bool
nullstellentest2 a2 a1 a0
                 | a2 == 0 && a1 == 0 && a0 /= 0 = False
                 | a2 /= 0 && (((a1/(2*a2))**2)-(a0/a2)) < 0 = False
                 | otherwise = True
				 
-- Teil c)
smallestValue :: Float -> Float -> Float -> Float
smallestValue a2 a1 a0
              | nullstellentest2 a2 a1 a0 = 0
              | (a2 == 0) && (a1 == 0) = abs a0 -- konstante Funktion /= 0
              | otherwise = abs ((a0 - ((a1**2)/(4*a2))))


-- Aufgabe 2)

-- Teil a)
cyclicShift :: Int -> Int -> Int -> Int -> Int
cyclicShift k n m s
            | (m >= k) && (m < k+n) = mod ((m-k)+s) (n+k)
            | otherwise = error "m out of range"

-- Teil b)
ceasarForCapitals :: Char -> Int -> Char
ceasarForCapitals b z
                  | isUpperCase b = toEnum (cyclicShift 65 26 (fromEnum b) z)
                  | otherwise = b 
 where
  isUpperCase :: Char -> Bool
  isUpperCase c = (fromEnum c >= fromEnum 'A') && (fromEnum c <= fromEnum 'Z')

-- Teil c)