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

l1 = [2,2,2,2,2,3]
l2 = [2,3,4,5,6,7,2,3,4,5]
l3 = [2,3,4,5]

todosIguales :: [Integer] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs)
    | x == head xs = todosIguales xs
    | otherwise = False

todosDistintosAux :: Integer -> [Integer] -> Integer
todosDistintosAux _ [] = 0
todosDistintosAux n [x] | n == x = 1
todosDistintosAux n xs
    | n == head xs = 1 + todosDistintosAux n (tail xs)
    | otherwise = todosDistintosAux n (tail xs)

todosDistintos :: [Integer] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos xs
    | todosDistintosAux (head xs) xs > 1 = False
    | otherwise = todosDistintos (tail xs)

quitar :: Integer -> [Integer] -> [Integer]
quitar _ [] = []
quitar n [x] | n == x = []
quitar n xs
    | n /= (head xs) = head xs : quitar n (tail xs)
    | otherwise = quitar n (tail xs)

sacarTodas :: [Integer] -> [Integer] -> [Integer]
sacarTodas xs [] = xs
sacarTodas [] _ = []
sacarTodas xs ys = sacarTodas (quitar (head ys) xs) (tail ys)

promedio :: [Float] -> Float
promedio [] = 0
promedio [x] = x
promedio xs = sum xs / fromIntegral (length xs)

notas :: Integer -> [(Integer, Float)] -> [Float]
notas _ [] = []
notas n [x] | n == fst x = [snd x]
notas n xs
    | n == fst (head xs) = [snd (head xs)] ++ notas n (tail xs)
    | otherwise = notas n (tail xs)

promedioDe :: [(Integer,Float)] -> Integer -> Float
promedioDe [] _ = 0
promedioDe xs n = promedio (notas n xs)