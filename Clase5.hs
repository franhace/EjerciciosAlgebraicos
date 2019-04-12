-- Implementar una función eAprox :: Integer -> Float que aproxime el valor del número
-- e a partir de la siguiente sumatoria:
-- import Clase4

factorial :: Integer -> Integer
factorial n | n == 1 = 1
	    | otherwise = n * factorial (n-1)

eAprox :: Integer -> Float 
eAprox n | n == 0 = 1
	 	 | otherwise = (1/fromInteger(factorial n)) + eAprox (n-1)

-- constante e :: Float
-- constante e = 2.71828
