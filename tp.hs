type Conjunto a = [a]
type Tablero = [[Integer]]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13, 12, 19, 6], [7, 13, 32, 6], [22, 20, 14, 7], [7, 33,53, 16], [27, 2, 8, 18]]

sopa2 :: Tablero
sopa2 = [[(-20), (-20), (-20)], [0, 10, 20], [(-10), (-10), 0], [10, 20,(-10)]]

sopa3 :: Tablero
sopa3 = [[10,5,15],[-1,7,2],[2,12,3]]

camino1 :: Camino
camino1 = [(1,1),(1,2),(2,2),(2,3)]

camino2 :: Camino
camino2 = [(2,1),(2,2),(2,3),(3,3),(4,3)]

camino3 :: Camino
camino3 = [(1,2),(2,2),(3,2)]

-- Dado la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Dado la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posicion de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)

-- Determina si una posicion esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t)

---- MaximoA
-- mezclaDos :: Ord a => [a] -> [a] -> [a]
mezclaDos [] ys         = ys
mezclaDos xs []         = xs
mezclaDos (x:xs) (y:ys)
  | x <= y    = x : mezclaDos    xs  (y:ys)
  | otherwise = y : mezclaDos (x:xs)    ys

-- mezclaTodos :: Ord a => [[a]] -> [a]
mezclaTodos (x:y:xs) = mezclaTodos ((mezclaDos x y) : xs)
mezclaTodos [x]      = x
mezclaTodos _        = []

-- compara de a dos de izq a derecha, y mueve el mayor hacia la derecha,
-- ordenarAux :: Integer -> [Integer] -> [Integer]
ordenarAux n [] = [n]
ordenarAux n (x:xs) | n <= x = n:ordenarAux x xs
                    | otherwise = x:ordenarAux n xs
-- chequea si ordenados
-- ordenados :: [Integer] -> Bool
ordenados [x] = True
ordenados (x:xs)  | x > head(xs) = False
                  | otherwise = ordenados xs

-- ordenar :: [Integer] -> [Integer]
ordenar [x] = [x]
ordenar (x:xs) | ordenados (x:xs) == True = (x:xs)
               | otherwise = ordenar (ordenarAux x xs)

-- busca_mayor :: [int] -> int
busca_mayor [x] = x
busca_mayor (x:xs) | (x > mezclaTodos xs) = x
                   | otherwise = busca_mayor xs

------------------- MaximoB


-- busca_menor :: [int] -> [int]
busca_menor [x] = x
busca_menor (x:xs) | (x < busca_menor xs) = x
                   | otherwise = busca_menor xs

-- borra_menor :: [int] -> [int]
borra_menor [] = []
borra_menor (x:xs) | (x == (busca_menor (x:xs))) = xs
                   | otherwise = (x:borra_menor xs)

-- aux_ordena :: [int] -> [int] -> [int]
aux_ordena lista_ordenada [] = lista_ordenada
aux_ordena lista_ordenada (x:xs) = aux_ordena (lista_ordenada++[busca_menor (x:xs)]) (borra_menor (x:xs))

-- ordena :: [integer] -> [integer]
ordena lista = aux_ordena [] lista






--------------------- Max repetido
-- primeroRepetido: compara un numero n con los elementos de una lista
-- primeroRepetido :: Integer -> [Integer] -> Bool
primeroRepetido _ [] = False
primeroRepetido n (x:xs) | n == x = True
                         | otherwise = primeroRepetido n xs

-- hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | primeroRepetido x xs = True
                    | otherwise = hayRepetidos xs
