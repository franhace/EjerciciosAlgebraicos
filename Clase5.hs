-- Implementar una función eAprox :: Integer -> Float que aproxime el valor del número
-- e a partir de la siguiente sumatoria:
-- import Clase4

factorial :: Integer -> Integer
factorial n | n == 1 = 1
                | otherwise = n * factorial (n-1)

eAprox :: Integer -> Float
eAprox n | n == 0 = 1
                 | otherwise = (1/fromInteger(factorial n)) + eAprox (n-1)

e :: Float
e = eAprox 100

-- Implementar una función parteEntera :: Float -> Integer que calcule la parte entera
-- de un número real positivo.
parteEnteraPositiva :: Float -> Integer
parteEnteraPositiva n | n < 1 = 0
                                          | otherwise = 1 + parteEnteraPositiva(n-1)

-- -- Cambiar la implementación de parteEntera :: Float -> Integer para que también
-- -- funcione con números negativos.
parteEnteraNegativa :: Float -> Integer
parteEnteraNegativa n | n >= 0 && n < 1  = 0
                       | n > 0     =  1 + parteEnteraNegativa (n-1)
                       | otherwise = -1 + parteEnteraNegativa (n+1)

parteEnteraNegativa2 :: Float -> Integer
parteEnteraNegativa2 n | n >= 0 && n < 1  = 0
                       | otherwise = signo(n)*1 + parteEnteraPositiva(n+((-1)*signo(n)))
                           where signo a | a < 0 = (-1)
                                         | otherwise = 1

-- Debe funcionar para a ≥ 0, d > 0 y no se pueden usar div , mod ni / .
division :: Integer -> Integer -> (Integer, Integer)
division a d | a < d  = (0, a)
                         -- cr = Conciente Y Resto
              | otherwise = (1 + fst(cr), snd(cr))
                         where cr = division (a-d) d

-- Extender la función division :: Integer -> Integer -> (Integer,Integer)
-- para que funcione para a ∈ Z, d > 0.
-- REVEER!!!
division2 :: Integer -> Integer -> (Integer, Integer)
division2 a d | signo a == 1 = division a d
                         -- cr = Conciente Y Resto
               | otherwise = ((-2) + signo a * fst(crn), snd(crn)+1)
                          where
                                signo a
                                          | a < 0 = (-1)
                                          | otherwise = 1
                                crn = division ((-a)-d) d

-- Implementar una función sumaDivisoresHasta :: Integer -> Integer -> Integer.
sumDivHasta :: Integer -> Integer -> Integer
sumDivHasta n k | k == 1 = 1
                | mod n k == 0 = k + recur 
                | otherwise = recur 
                where recur = sumDivHasta n (k-1) 

-- Implementar la función sumaDivisores en función de la anterior.
sumDiv :: Integer -> Integer
sumDiv n = sumDivHasta n n

-- otra forma mas prolija
-- menorDivisor :: Integer -> Integer que calcule el menor divisor (mayor que 1) de un natural n.

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n-k == 0 = k
                      | mod n k /= 0 = menorDivisorDesde n (k+1)
		      | otherwise = k

menorDivisorDesde2 :: Integer -> Integer -> Integer
menorDivisorDesde2 n k | mod n k == 0 = k
                       | otherwise = menorDivisorDesde2 n (k+1)

menorDivisor :: Integer -> Integer
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorDesde n 2

-- Implementar la función esPrimo :: Integer -> Bool .
esPrimo :: Integer -> Bool
esPrimo n = n == (menorDivisor n)

-- Implementar la siguiente función: doble sumatoria 
-- la primera de i= 1 hasta n
-- la segunda desde j= 1 hasta m
-- de i**j
dobleSuma n m i j | i == 1 = 1
		  | j == 1 = i
		  | otherwise = i^j + dobleSuma n m i (j-1)


--Locuras con Fibo
fibo :: Integer -> Integer
fibo n  | n == 0 = 0
        | n == 1 = 1
        | otherwise = fibo (n-1) + fibo (n-2)
-- 2 -> 0 1 1
fiboHasta :: Integer -> Integer
fiboHasta n | n == 0 = 0
                        | otherwise = fibo n + fibo (n-1)

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
main = print $ fibs !! 5
x
