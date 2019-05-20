-- Listas
-- Tienen que ser con elementos del mismo tipo

listita = [2, 3, 4, 5, 3, 2, 9, 12]

--Definir la función listar :: a -> a -> a -> [a] que toma 3 elementos y los convierte
--en una lista
listar :: a -> a -> a -> [a]
listar a b c = [a, b, c]

--Definir la función pertenece :: a -> [a] -> Bool que indica si el elemento que se le pasa
--está o no en la lista.

-- Forma 1
-- x `elem` xs

-- Forma 2
pertenece :: Integer -> [Integer] -> Bool
pertenece _ [] = False
pertenece b (x:xs) = (b == x) || pertenece b xs

-- sumatoria :: [Integer] -> Integer
-- que indica la suma de los elementos de una lista.

--sumatoria :: [Integer] -> Integer
--sumatoria xs = xs[0]

--pertenece :: Integer -> [Integer] -> Bool
--que indica si un elemento aparece en la lista. Por ejemplo:
--pertenece 9 [] False
--pertenece 9 [1,2,3] False
--pertenece 9 [1,2,9,9,-1,0] True

--pertenece :: Integer -> [Integer] -> Bool
