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
