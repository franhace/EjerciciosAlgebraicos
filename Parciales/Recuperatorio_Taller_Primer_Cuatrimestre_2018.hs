-- Ej 1
bisiesto :: Integer -> Bool
bisiesto n | mod n 400 == 0 = True
           | mod n 100 == 0 = False
           | mod n 4 == 0 = True
           | otherwise = False

-- Ej 2
sterling :: Integer -> Integer -> Integer
sterling n k| n == k || k == 1 = 1
            | otherwise = k * (sterling (n-1) k) + sterling (n-1) (k-1)

-- Ej 3
sucesion :: Integer -> Integer
sucesion n | mod n 2 == 0 = div n 2
           | otherwise = n +1

composicion :: Integer -> Integer
composicion n | n == 1 = 0
              | otherwise = composicion (sucesion n) + 1

-- Ej 4

-- Dice si un numero es el cuadrado del otro
esCuadrado :: Integer -> Integer -> Bool
esCuadrado n m | n == m^2 = True
                | otherwise = False

-- Funcion que toma 2 listas, y compara por posiciones si una es el cuadrado de la otra
lugarCuadradoAux :: [Integer] -> [Integer] -> [Integer]
lugarCuadradoAux _ [] = []
lugarCuadradoAux [] _ = []
lugarCuadradoAux [x] [j]
    | esCuadrado x j = [x]
    | otherwise = []
lugarCuadradoAux (x:xs) (j:js)
    | esCuadrado x j == True = x : lugarCuadradoAux (xs) (js)
    | otherwise = lugarCuadradoAux (xs) (js)


-- Da una lista con indices, teniendo en cuenta largo de la lista
listaIndices :: [Integer] -> [Integer]
listaIndices [] = []
listaIndices [x] = [1]
listaIndices xs = [1..x]
            where x = toInteger (length xs)

-- Devuelve la cantidad de elementos de una lista que sean iguales al cuadrado de su posicion
lugarCuadrado :: [Integer] -> Integer
lugarCuadrado [] = 0
lugarCuadrado (xs) = toInteger (length(lugarCuadradoAux xs (listaIndices xs)))

-- Ej 5
-- Devuelve una lista, con todos los elementos igual al elemento
apariciones :: Integer -> [Integer] -> [Integer]
apariciones _ [] = []
apariciones n (x:xs)
    | n == x = x:(apariciones n xs)
    | otherwise = (apariciones n xs)

-- Devuelve la cantidad de apariciones de un elemento en una lista
cantidadDeApariciones :: Integer -> [Integer] -> Integer
cantidadDeApariciones _ [] = 0
cantidadDeApariciones n (x:xs) = toInteger(length (apariciones n (x:xs)))

-- tupla 2 elementos disjuntos
tuplar :: Integer -> Integer -> (Integer,Integer)
tuplar a b = (a,b)

l = [2,2,2,5,5]

-- Elimina todas las apariciones de un elemento
quitarTodas :: Integer -> [Integer] -> [Integer]
quitarTodas _ [] = []
quitarTodas n (x:xs)
    | n /= x = x:(quitarTodas n xs)
    | otherwise = quitarTodas n xs

-- Tupla (elemento de la lista, cantidad de apariciones)
tuplador :: [Integer] -> [(Integer, Integer)]
tuplador [] = []
tuplador [x] = [(x,1)]
tuplador (xs) = (head xs, (cantidadDeApariciones (head xs) xs )) : tuplador (quitarTodas (head xs) (tail xs))

-- Las 5 funciones anteriores se pueden resumir en comprimir (funcion del simulacro)..
-- Pero me gusta mi funcioncita

l2 = [[5,7,8], [], [5,8,4], [5]]

flat::[[a]] -> [a]
flat [] = []
flat (l:ls) = l ++ (flat ls)

-- Solo guarda elementos cuya cantidad de apariciones sea impar, en una lista
comprimirAux :: [(Integer, Integer)] -> [Set Integer]
comprimirAux [] = []
comprimirAux [(a,b)]
    | mod b 2 /= 0 = [a]
    | otherwise = []
comprimirAux ((a,b):xs)
  | (mod b 2 == 0) = comprimirAux xs
  | otherwise = a : comprimirAux xs

-- Este deberia ser el dif sim
--altoCompresor :: [Set Integer] -> Set Integer
--altoCompresor [xs] = comprimirAux (tuplador (flat xs))
