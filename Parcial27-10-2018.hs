-- Ejercicio 1 --
-- dada una tupa abc de enteros y un entero n, devuelve
-- V si 2 de las coord de la tupla suman n
suma2 :: (Integer, Integer, Integer) -> Integer -> Bool
suma2 (a, b, c) n | (a + b == n) || (a + c == n) || b + c == n = True
                  | otherwise = False

-- Ejercicio 2 --
sumatoria :: Integer -> Integer
sumatoria 1 = 1
sumatoria n = (2*n - 1)^2 + sumatoria (n-1)

-- Ejercicio 3 --
--Todos iguales
todosIguales :: [Integer] -> Bool
todosIguales [] = True
todosIguales (x:[]) = True
todosIguales (x:xs) | x /= head(xs) = False
                    | otherwise = todosIguales xs

-- Todos distintos
-- funcion aux que devuelve una lista con las apariciones de n
apariciones :: Integer -> [Integer] -> [Integer]
apariciones _ [] = []
apariciones n (x:xs) | n == x = x:(apariciones n xs)
                     | otherwise = (apariciones n xs)

-- funcion aux que devuelve True si un elemento se repite
hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | length(apariciones x xs) > 0 = True
                    | otherwise = hayRepetidos xs

todosDistintos :: [Integer] -> Bool
todosDistintos (x:xs) | hayRepetidos (x:xs) == True = False
                      | otherwise = True

-- Ejercicio 4 --
--sacarTodos :: [Integer] -> [Integer] -> [Integer]
-- que deja en la lista una única aparición
--de cada elemento, eliminando las repeticiones adicionales.
quitar1 :: Integer -> [Integer] -> [Integer]
quitar1 _ [] = []
quitar1 n (x:xs) | x == n = xs
				 | otherwise = (quitar1 n xs)

-- Elimina todas las apariciones de un elemento
quitarTodas :: Integer -> [Integer] -> [Integer]
quitarTodas _ [] = []
quitarTodas n (x:xs) | n /= x = x:(quitarTodas n xs)
				     | otherwise = quitarTodas n xs


eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | hayRepetidos (x:xs) == False = (x:xs)
						 | hayRepetidos (x:xs) == True && length (apariciones x (xs)) > 0 = x : eliminarRepetidos (quitarTodas x (xs))
						 | otherwise = x:eliminarRepetidos xs