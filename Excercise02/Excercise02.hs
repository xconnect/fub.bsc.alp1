module Excercise02 where

{-
procedure logFloor(n,b):
	x := 1
	while (x<n) do
		x := x*b;
	return (x-1);
-}

-- Aufgabe 2)

-- >> WHILE Schleife wird in eine endrekursive Funktion überführt << --

-- Berechnung des abgerundeten Logarithmus
type Basis = Int
type Nummer = Int

logFloor :: (Nummer, Basis) -> Int
logFloor (n, b)
         | n <= 0 || b <= 1 = error "Nicht definiert!"
		 | otherwise = (logi n b 1) - 1
	where
		logi :: Int -> Int -> Int -> Int
		logi n b x
			 | x < n = 1 + logi n b (x*b)
			 | otherwise = 0

-- Aufgabe 3)

-- testet, ob Eingabe eine Primzahl ist
primTest :: Int -> Bool
primTest n = null [ k | k <- [2..n-1],  mod n k == 0]

-- testet, ob x durch y oder y durch x teilbar ist
teilbarkeit :: Int -> Int -> Bool
teilbarkeit x y = mod x y == 0 || mod y x == 0