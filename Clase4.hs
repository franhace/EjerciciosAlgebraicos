
module Clase4
where

import Clase3

esPar n | mod n 2 == 0 = True
        | otherwise = False

-- Funciones recursivas

fibo :: Int -> Int
fibo n | n == 0 = 0
       | n == 1 = 1
       | otherwise = fibo (n-1) + fibo (n-2) 

-- Sucesiones, devuelve el n-esimo termino de la suc

suce1 :: Integer -> Integer 
suce1 n | n == 1 = 2
        | otherwise = 2 * (n-1) * suce1 (n - 1) + ((2 ^ (n+1)) * factorial n)

 
suce2 :: Integer -> Integer
suce2  n | n == 1 = 1
         | n == 2 = 2
         | otherwise = n * suce2 ( n-1 ) + 2 * (n+1) * suce2 ( n-2)

suce3 :: Integer -> Integer
suce3 n | n == 1 = -3
        | n == 2 = 6
        | esPar n = suce3 ( n-1 ) + 2 * suce3 (n-2) + 9
        | otherwise = ((suce3 (n-1)) * (-1)) -3

sumatoria :: Integer -> Integer 
sumatoria n | n == 0 = 0
	    | n > 0 = n + sumatoria(n-1)
	    | otherwise = 0

f1 :: Int -> Int
f1 n | n == 0 = 1
     | n > 0 = 2 ^ n + f1 (n-1)	

f2 :: Int -> Float -> Float
-- para que funque para f3 reemplazar la linea de abajo por 0, sino 1
f2 n q  | n == 1 = 0
	| otherwise = q ^ n + f2 (n-1) q

f3 :: Int -> Float -> Float
f3 n q  | n == 0 = 1
	| n > 0 = q ^ (2*n) + f3 (2*(n-1)) q

f3b :: Int -> Float -> Float
f3b n q = f2 ( 2 * n ) q

f4 :: Int -> Float -> Float
f4 n q  | n == 0 = 1
	| n == 1 = q
	| n > 1 = q ^ (2*n) - f4 (n-1) q

f4b :: Int -> Float -> Float
f4b n q = (f3 n q) - (f2 n q)

-- Funcion par sin usar mod ni div

esPar3 :: Int -> Bool
esPar3 n | n == 0 = True
         | n < 2  = False
         | otherwise = esPar3 (n-2)

esMultiploDe3 :: Int -> Bool
esMultiploDe3 n | n == 0 = True
		| n < 3 = False
		| otherwise = esMultiploDe3 (n-3)

-- Implementar la función sumaImpares :: Integer -> Integer que dado n ∈ N sume los
--primeros n números impares. Ej: sumaImpares 3 ( 1+3+5 = 9).
sumaImpares :: Integer -> Integer
sumaImpares n | n == 0 = 0	      
	      | otherwise = ( 2 * n -1) + sumaImpares (n-1)

-- Escribir una función mediofact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · -- Por ejemplo: doblefact 10 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 = 3840.
mediofact :: Integer -> Integer
mediofact n | n <= 0 = 1
	    | otherwise = n * mediofact (n-2) 


-- Escribir una función recursiva que no termine si se la ejecuta con enteros negativos (y en cambio sı́ termine para el resto de los enteros)































