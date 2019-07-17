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
sucesion n = 1 + suceAux n

suceAux :: Integer -> Integer
suceAux 1 = 1
suceAux n = suceAux (n-1)

-- Ej 3
--isInt x = x == fromInteger (round x)
--esCuadrado :: Integer -> Bool
--esCuadrado n = 
-- es suma de 2 cuadrados = es suma de 2 primos

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

menorDivisor :: Integer -> Integer
menorDivisor n | n == 1 = 1
               | otherwise = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n | n == 1 = False
          | otherwise = n == (menorDivisor n)

esSumaDeDosPrimosDesde :: Integer -> Integer -> Bool
esSumaDeDosPrimosDesde n p | p > (n-p ) = False
						| esPrimo p && esPrimo (n-p) = True
						| otherwise = esSumaDeDosPrimosDesde n (p+1)

checkPrimos :: Integer -> Integer -> Bool
checkPrimos 1 _ = False
checkPrimos n k | (esPrimo n) && (esPrimo k) = True
                | otherwise = checkPrimos (n-1) (k+1)

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos 1 = False
esSumaDeDosPrimos n = checkPrimos (n-1) 1

-- Ej 4
esPositivo :: Integer -> Bool
esPositivo n = n >0
			 
cambioSigno :: Integer -> Integer -> Bool
cambioSigno u v = esPositivo u /= esPositivo v 

-- Fx que cuenta cuantos cambios de signo hay
reglaDescartes :: [Integer] -> Integer
reglaDescartes [] = 0
reglaDescartes [x] = 0
reglaDescartes [x,j] | cambioSigno x j == True = 1
					 | otherwise = 0
reglaDescartes (x:j:xs) | cambioSigno x j == True = 1 + reglaDescartes (j:xs)
						| otherwise = reglaDescartes (j:xs)

-- Ej 5
--esSemiPerfecto :: Integer -> Bool 
