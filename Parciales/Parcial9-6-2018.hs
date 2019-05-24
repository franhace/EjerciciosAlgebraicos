-- Ej 1
-- Funcion que compara numeros por sus decenas
-- y devuelve el minimo

-- Fx que busca la decena
decenas :: Integer -> Integer
decenas n = div (mod n 100) 10

minimo :: (Integer, Integer, Integer) -> Integer
minimo (a,b,c) | decenas a < decenas b && decenas a < decenas c = a
               | decenas a < decenas b && decenas c < decenas a = c
               | otherwise = b

-- Ej 2
sucesion :: Integer -> Integer
sucesion 1 = 1
sucesion n = 1 + n*(sucesion(n-1)) 

-- Ej 3
isInt x = x == fromInteger (round x)
--esCuadrado :: Integer -> Bool
--esCuadrado n | (sqrt n) * 10  == isInt n = True
--	     | otherwise = False
	     

-- esSumaDeCuadrados :: Integer -> Bool

