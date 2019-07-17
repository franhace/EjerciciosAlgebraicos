-- Ejercicio 1
-- Implementar la función menorLex que dados dos vectores x, y ∈ R3
-- decida si x es menor a y en el sentido lexicográfico
-- es decir,x < y si la primera coordenada de x es menor que la primera
-- coordenada deyo son iguales y se satisface esto mismo para el resto del vector.
-- Por ejemplo:menorLex (3,-1,2) (5,10,0)True(pues3<5)
-- menorLex (4,-1,7) (4,21,5)True(pues coinciden en la primera coordenada, y−1<21)
-- menorLex (2,1,31) (2,1,-5)False(pues coinciden en las dos primeras coordenadas, pero31≮−5)

menorLex :: (Float,Float,Float) -> (Float,Float,Float) -> Bool
menorLex (a, b, c) (d, e, f) | a < d = True
                             | a >= d && b < e = True
                             | a >= d && b >= e && c < f = True
                             | otherwise = False

--Ejercicio 2
-- Implementar una función Fibo que para cada n≥1 calcule ∑n j=0 fn,
-- donde fnesn–ésimo término de la sucesión de Fibonacci
sumaFibonacci :: Integer->Integer
sumaFibonacci 0 = 0
sumaFibonacci 1 = 1
sumaFibonacci n = sumaFibonacci(n-1) + sumaFibonacci (n-2)

sumaFibo :: Integer -> Integer
sumaFibo 0 = 1
sumaFibo n = sumaFibo (n) + sumaFibo (n-1)
--

-- Ejercicio 3
-- Implementar una función sumaDivisoresHasta :: Integer -> Integer -> Integer.
-- Esta fx devuelve la suma de los divisores de un número hasta cierto punto.
sumDivHasta :: Integer -> Integer -> Integer
sumDivHasta n k | k == 1 = 1
                | mod n k == 0 = k + recur
                | otherwise = recur
                where recur = sumDivHasta n (k-1)

-- Implementar la función sumaDivisores en función de la anterior.
esDefectivo :: Integer-> Bool
esDefectivo n | sumDivHasta n (n-1) <= n = True
              | otherwise = False

-- Ejercicio 4
-- Programe la función maxDistancia, que determina cuál es la máxima distancia
-- entre: dos elementos consecutivos en una lista de números enteros
--maximaDistancia :: [Integer] -> Integer
--maximaDistancia (x:xs)

gap2 :: Integer -> Integer -> Integer
gap2 a b = masGrande - masChico
          where masGrande = max a b
                masChico = min a b

maximaDistancia :: [Integer] -> Integer
maximaDistancia [] = 0
maximaDistancia [x] = 0
maximaDistancia [x,j] = gap2 x j
maximaDistancia (x:j:xs) | gap2 x j >= gap2 j (head xs) = maximaDistancia (x:xs)
                         | otherwise = maximaDistancia (j:xs) 
                         
                         
maximaDistancia2 :: [Integer] -> Integer
maximaDistancia2 [a,b] = abs (a - b)
maximaDistancia2 (x1:x2:xs)
  | dist > maximaDistancia2 (x2:xs) = dist
  | otherwise = maximaDistancia2 (x2:xs)
    where dist = abs (x1 - x2)

-- Ejercicio 5
-- Dada una lista devuelve la cantidad de repeticiones de los numeros que aparecen
-- En una lista de tuplas
apariciones :: Integer -> [Integer] -> [Integer]
apariciones _ [] = []
apariciones n (x:xs) | n == x = x:(apariciones n xs)
                     | otherwise = (apariciones n xs)

cantidadDeApariciones :: Integer -> [Integer] -> Int
cantidadDeApariciones _ [] = 0
cantidadDeApariciones n (x:xs) = length (apariciones n (x:xs))


-- 
--comprimirAux :: [(Integer, Integer)] -> [(Integer, Integer)]
--comprimirAux []= []
--comprimirAux ((x,y)(a,b):xs) | x == a = comprimirAux ((x,y+b):xs)
--                             | otherwise = (x,y):comprimirAux((a,b):xs)

tuplar :: [Integer] -> [(Integer,Integer)]
tuplar [] = []
tuplar (x:xs) = (x,1) : tuplar xs

--comprimir :: [Integer] -> [(Integer,Integer)]
--comprimir xs = comprimirAux(tuplar xs)

-- EsCAPICUA

-- devuelve los digitos de un integer, separados en una lista
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

esCapicuaAux :: [Integer] -> Bool
esCapicuaAux (xs)
    | length xs <= 1 = True
    | head xs /= last xs = False
    | otherwise = esCapicuaAux (init (tail xs))

esCapicua :: Integer -> Bool
esCapicua a = esCapicuaAux (digs a)

esCapi :: Integer -> Bool
esCapi x = (div x 1000 == mod x 10) && (div w 10 == mod w 10)
        where w = rem x 1000 `quot` 10

suce :: Integer -> Integer
suce 1 = 1
suce 2 = 2
suce 3 = 5
suce n = 3*(suce (n-1))^2 + 2*(suce(n-2)) + suce (n-3)

listarSuce :: Integer -> [Integer]
listarSuce n = [suce n] ++ listarSuce (suce (n+1))

esTerminoDeSuc :: Integer -> Integer -> Bool
esTerminoDeSuc n m
    | (suce m) == n = True
    | (suce m ) > n = False
    | (suce m) < n = esTerminoDeSuc n (m+1)

listaConTerminosSuc :: [Integer] -> [Integer]
listaConTerminosSuc [] = []
listaConTerminosSuc (x:xs)
    | (esTerminoDeSuc x 1 == True) = x : listaConTerminosSuc xs
    | otherwise = listaConTerminosSuc xs

sumaTerminos :: [Integer] -> Float
sumaTerminos [] = 0
sumaTerminos (x:xs) = fromInteger x + sumaTerminos xs

ll = [5,23489,5432,1,19212,345]