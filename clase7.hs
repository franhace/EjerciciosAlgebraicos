-- Listas
-- Tienen que ser con elementos del mismo tipo

listita = [2, 3, 4, 5, 3, 2, 9, 12]

--Ej: Definir la función listar :: a -> a -> a -> [a] que toma 3 elementos y los convierte
--en una lista
listar :: a -> a -> a -> [a]
listar a b c = [a, b, c]

--Ej: Definir la función pertenece :: a -> [a] -> Bool que indica si el elemento que se le pasa
--está o no en la lista.

-- Forma 1
-- x `elem` xs

-- Forma 2
pertenece :: Integer -> [Integer] -> Bool
pertenece _ [] = False
pertenece b (x:xs) = (b == x) || pertenece b xs

-- Ej: sumatoria :: [Integer] -> Integer
-- que indica la suma de los elementos de una lista.
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- Funcion que le suma un numero a cada valor de una lista
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (x+n):(sumarN n xs)

-- Funcion que suma 1 a cada valor de una lista
crece :: [Integer] -> [Integer]
crece [] = []
crece (x:xs) = (x+1) : crece  xs

-- Funcion que resta cada vez un numero mas grande
decre :: [Integer] -> [Integer]
decre [] = []
decre (x:xs) = (x-1) : decre xs

sumatorio :: Integer -> Integer -> [Integer]
sumatorio a b
    |a >= b = []
    |otherwise = crece listar
    where listar = [a..b]


--Ej: Escribir una expresión que denote la lista estrictamente decreciente de enteros que comienza
--con el número 1 y termina con el número -100.

decreciente :: Integer -> [Integer] -> [Integer]
decreciente _ [] = []
decreciente a (x:xs) = (x-a) : (decreciente a xs)

--sumatoria :: [Integer] -> Integer
--sumatoria xs = xs[0]

--pertenece :: Integer -> [Integer] -> Bool
--que indica si un elemento aparece en la lista. Por ejemplo:
--pertenece 9 [] False
--pertenece 9 [1,2,3] False
--pertenece 9 [1,2,9,9,-1,0] True

--pertenece :: Integer -> [Integer] -> Bool

--Definir la función primerMultiplode45345 :: [Integer] -> Integer que indica el
--primer elemento de la lista que es múltiplo de 45345 que encuentre en la lista. Definir la
--función para que tome listas infinitas.
--primerMultiplode45345 :: [Integer] -> Integer
--primerMultiplode45345 (x:xs) | x mod 45345 == 0 = x
--							 | otherwise = primerMultiplode45345 xs 

-- Pattern matching
longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- productoria que devuelve la productoria de los elementos.
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = productoria xs * x

-- dado un número N y una lista xs,suma N a cada elemento de xs.
sumarN2 :: Integer -> [Integer] -> [Integer]
sumarN2 _ [] = []
sumarN2 n (x:xs) = (x + n) : (sumarN2 n xs)

--que dada una lista no vacı́a xs, suma el
--primer elemento a cada elemento de xs. Ejemplo sumarElPrimero [1,2,3]
--[2,3,4]
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN primero xs
					where primero = head xs

-- sumarElUltimo :: [Integer] -> [Integer] que dada una lista no vacı́a xs, suma el
--último elemento a cada elemento de xs. Ejemplo sumarElUltimo [1,2,3]
--[4,5,6]

-- Busca el ultimo de una lista
ultimo :: [Integer] -> Integer
ultimo [x] = x
ultimo (x:xs) = ultimo xs

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN2 ultime xs
				where ultime = ultimo xs
				
-- Pares devuelve una lista con los elementos pares de la
--lista original. Ejemplo pares [1,2,3,5,8]
--[2,8]
esPar :: Integer -> Bool
esPar n | mod n 2 == 0 = True
		| otherwise = False

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | esPar x == True = (x) : (pares xs)
			 | otherwise = pares (xs)

parbis :: [Integer] -> [Integer]
parbis [] = []
parbis (x:xs) | mod x 2 == 0 = x:(parbis xs)
			  | otherwise = parbis xs

--Dado un número N y una
--lista xs, devuelve una lista con los elementos multiplos N de xs.
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x:(multiplosDeN n xs)
					  | otherwise = multiplosDeN n xs

-- Elimina la primera aparición del elemento en la lista (de haberla).
quitar1 :: Integer -> [Integer] -> [Integer]
quitar1 _ [] = []
quitar1 n (x:xs) | x == n = xs
				 | otherwise = (quitar1 n xs)

-- Elimina todas las apariciones de un elemento
quitarTodas :: Integer -> [Integer] -> [Integer]
quitarTodas _ [] = []
quitarTodas n (x:xs) | n /= x = x:(quitarTodas n xs)
				     | otherwise = quitarTodas n xs

-- Indica si una lista tiene elementos repetidos.
apariciones :: Integer -> [Integer] -> [Integer]
apariciones _ [] = []
apariciones n (x:xs) | n == x = x:(apariciones n xs)
					 | otherwise = (apariciones n xs)
 
hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | length (apariciones x xs) > 0 = True
					| otherwise = hayRepetidos xs

lis = [2, 3, 3, 4, 4, 5, 5,1]

-- que deja en la lista una única aparición
--de cada elemento, eliminando las repeticiones adicionales.
eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | hayRepetidos (x:xs) == False = (x:xs)
						 | hayRepetidos (x:xs) == True && length (apariciones x (xs)) > 0 = x : eliminarRepetidos (quitarTodas x (xs))  
						 | otherwise = x:eliminarRepetidos xs

--Calcula el máximo elemento de una lista no vacı́a.
maxAux :: Integer -> [Integer] -> Integer
maxAux n [] = n
maxAux n (x:xs) | n >= x = maxAux n xs
				| otherwise = maxAux n xs 

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) = maxAux x xs


maximo2 :: [Integer] -> Integer
maximo2 [x] = x
maximo2 (x:xs)
    | x > maximo2 xs = x
    | otherwise = maximo xs

--Ordena los elementos de forma creciente.

ordenados :: [Integer] -> Bool
ordenados [x] = True
ordenados (x:xs) | x > head(xs) = False
                 | otherwise = ordenados xs

-- compara de a dos de izq a derecha, y mueve el mayor hacia la derecha,
ordenarAux :: Integer -> [Integer] -> [Integer]
ordenarAux n [] = [n]
ordenarAux n (x:xs) | n <= x = n:ordenarAux x xs
					| otherwise = x:ordenarAux n xs

-- reversa una lista
reversar :: [Integer] -> [Integer]
reversar [] = []
reversar (x:xs) = reversar xs ++ [x]