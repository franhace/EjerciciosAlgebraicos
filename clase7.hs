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
sumarN n (x:xs) = (x-1):(sumarN n xs)

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
