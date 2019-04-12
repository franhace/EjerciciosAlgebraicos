-- Implementar una función eAprox :: Integer -> Float que aproxime el valor del número
-- e a partir de la siguiente sumatoria:
-- import Clase4

factorial :: Integer -> Integer
factorial n | n == 1 = 1
	    | otherwise = n * factorial (n-1)

eAprox :: Integer -> Float 
eAprox n | n == 0 = 1
	 	 | otherwise = (1/fromInteger(factorial n)) + eAprox (n-1)

--constante :: Float
-- constante e = eAprox 100
-- 0 1 1 2 3 5 8 
fibo :: Integer -> Integer
fibo n | n == 0 = 0
       | n == 1 = 1
       | otherwise = fibo (n-1) + fibo (n-2) 
-- 2 -> 0 1 1 
fiboHasta :: Integer -> Integer
fiboHasta n | n == 0 = 0
			| otherwise = fibo n + fibo (n-1)

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
main = print $ fibs !! 5