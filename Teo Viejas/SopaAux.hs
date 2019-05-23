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

--Devuelve el máximo de un tablero.
maximo :: Tablero -> Integer
maximo (t:[]) = maximum (t)
maximo (t:y:ys)   | maximum t >= maximum y = maximo (t:ys)
				  				| otherwise = maximo (y:ys)

--Devuelve el elemento más repetido de un tablero
masRepetido :: Tablero -> Integer
masRepetido (t:ts) = masRepetidoAux (listaGeneral (t:ts)) (0,0) (0,0)

------masRepetido funciones auxiliares------

--Devuelve una lista con todos los numeros de un tablero
listaGeneral :: Tablero -> [Integer]
listaGeneral [] = []
listaGeneral (x:xs) = ordenar (x ++ listaGeneral xs)


--Elimina el head de una lista si es igual al numero n
quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs) | n == x = xs
            	| n /= x = x : quitar n xs

--Ordena una lista de menor a mayor
ordenar :: [Integer] -> [Integer]
ordenar xs | xs == [] = []
           | otherwise = ordenar (quitar (maximum xs) xs) ++ [maximum xs]

--Encuentra al elemento más repetido de una lista
masRepetidoAux :: [Integer] -> (Integer, Integer) -> (Integer, Integer) -> Integer
masRepetidoAux (x:[]) (a, b) (c, d) = a
masRepetidoAux (x:y:ys) (a, b) (c, d) | b == 0 && d == 0 = masRepetidoAux (y:ys) (x, b+1) (c, d)
								   	  | a == y && b /= 0 = masRepetidoAux (y:ys) (a, b+1) (c, d)
								   	  | a /= y && d == 0 = masRepetidoAux (y:ys) (a, b) (y, d+1)
								   	  | c == y && d /= 0 && d <= b = masRepetidoAux (y:ys) (a, b) (c, d+1)
								   	  | c /= y && d /= 0 && d <= b = masRepetidoAux (y:ys) (a, b) (0, 0)
								   	  | d > b = masRepetidoAux (y:ys) (x, d) (0, 0)
--------------------------------------------

--Devuelve los numeros de los casilleros del camino
numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino (t:ts) [x] = [valor(t:ts) x]
numerosDeCamino (t:ts) (x:xs) = valor (t:ts) x : numerosDeCamino (t:ts) xs


--Devuelve True si y solo si en un camino no aparecen numeros repetidos
caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos (t:ts) (x:xs) = not (hayRepetidos (numerosDeCamino (t:ts) (x:xs)))

------caminosSinRepetidos funciones auxiliares------

--Devuelve True si y solo si algun elemento de la lista se repite
hayRepetidos :: [Integer] -> Bool
hayRepetidos [x] = False
hayRepetidos (x:xs) = elem x xs || hayRepetidos (xs)
------------------------------------------

--Devuelve True si y solo si el camino es de Fibonacci
caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci (t:ts) (x:xs) | length (x:xs) < 3 = True
								| length (x:xs) >= 3 && primerElemento + segundoElemento /= tercerElemento = False
								| otherwise = caminoDeFibonacci (t:ts) xs
				where
					primerElemento = head(numerosDeCamino (t:ts) (x:xs))
					segundoElemento = head(tail(numerosDeCamino (t:ts) (x:xs)))
					tercerElemento = head(tail(tail(numerosDeCamino (t:ts) (x:xs))))

test :: Tablero -> [(Integer, Integer)]
test (t:ts) = aCoordenadas (t:ts) (1, 1)

------mayorSecuenciaDeFibonacci funciones auxiliares------
type Set a = [a]

aCoordenadas :: Tablero -> (Integer, Integer) -> [(Integer, Integer)]
aCoordenadas [] (n, k) = []
aCoordenadas (t:ts) (n, k) | toInteger (length t) >= k = [(n, k)] ++ aCoordenadas (t:ts) (n, k+1)
					 	   | toInteger (length t) < k = aCoordenadas (ts) (n+1, 1)

caminosDeFibonacci2 :: Tablero -> [Camino] -> [[Integer]]
caminosDeFibonacci2 (t:ts) [[]] =[]
caminosDeFibonacci2 (t:ts) (x:xs) | caminoDeFibonacci (t:ts) x == True = [numerosDeCamino (t:ts) x] ++ caminosDeFibonacci2 (t:ts) xs
								  | otherwise = caminosDeFibonacci2 (t:ts) xs


caminoValido :: [(Integer, Integer)] -> Bool
caminoValido (x:[]) = True
caminoValido (x:y:ys) | ((fst y + snd y) - (fst x + snd x)) == 1 = caminoValido (y:ys)
					  | otherwise = False

partes :: (Integer, Integer) -> [[(Integer, Integer)]]
partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = partes (n-1) ++ agregarATodos n (partes (n-1))

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos _ [] = []
agregarATodos n (c:cls) = (agregar n c) : (agregarATodos n cls)

agregar :: Integer -> Set Integer -> Set Integer
agregar n cs | elem n cs = cs
             | otherwise = ordenar (n : cs)

caminosDeFibonacci2 :: Tablero -> [Camino] -> [[Integer]]
caminosDeFibonacci2 (t:ts) [] = []
caminosDeFibonacci2 (t:ts) (x:xs) | caminoDeFibonacci (t:ts) x == True = [numerosDeCamino (t:ts) x] ++ caminosDeFibonacci2 (t:ts) xs
						                   		| otherwise = caminosDeFibonacci2 (t:ts) xs

agregar :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
agregar x cls | elem x cls =  cls
							| otherwise = (x : cls)

agregarATodos :: (Integer, Integer) -> [[(Integer, Integer)]] -> [[(Integer, Integer)]]
agregarATodos _ [] = []
agregarATodos n (c:cls) =  (agregar n c) : (agregarATodos n cls)


"necesito que el partes reciba la lista de coordenadas devuelva las partes pero con esas coordenadas, por ejemplo si pongo partes de 2 que me devuelva [[(1,1)],[(1,2)],[(1,1),(1,2)]]"

agregar :: Integer -> Set Integer -> Set Integer
agregar n cs | elem n cs = ordenar cs
             | otherwise = ordenar (n : cs)

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos _ [] = []
agregarATodos n (c:cls) = (ordenar (agregar n c) : (agregarATodos n cls)

partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = partes (n-1) ++ agregarATodos n (partes (n-1))

agregar :: Integer -> [(Integer, Integer)] -> Set (Integer, Integer) -> Set (Integer, Integer)
agregar n xs cs | elem (n !! xs) cs = cs
                | otherwise = ordenar ((n !! xs) : cs
