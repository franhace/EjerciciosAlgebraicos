--Apellido y nombre               LU o DNI
--Larregui Nicolás Alejandro      133/18
--Sosa Móttola Eduardo Nicolás    657/18
--Acha Francisco                  36400171

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

-- Devuelve el máximo de una lista de enteros
maximoLista :: [Integer] -> Integer
maximoLista [x] = x
maximoLista (x:xs) | x >= (head xs) = maximoLista (x:(tail xs))
                   | x < (head xs) = maximoLista xs

-- Devuelve el máximo de un tablero.
maximo :: Tablero -> Integer
maximo (t:[]) = maximoLista (t)
maximo (t:y:ys)   | maximoLista t >= maximoLista y = maximo (t:ys)
				  | otherwise = maximo (y:ys)

-- Cuenta la cantidad de veces que es repetido cierto número de una lista
cuentoRepetidos :: Integer -> [Integer] -> Integer
cuentoRepetidos k [] = 0
cuentoRepetidos k (x:xs) | k == x = 1 + cuentoRepetidos k xs
                         | otherwise = 0 + cuentoRepetidos k xs

-- Verifica el número más repetido dentro de una lista
masRepetidoAux :: [Integer] -> Integer
masRepetidoAux [x] = x
masRepetidoAux (x:xs) | length (x:xs) == 2 && cuentoRepetidos x (x:xs) >= cuentoRepetidos (head xs) (x:xs) = x
                      | length (x:xs) == 2 && cuentoRepetidos x (x:xs) <= cuentoRepetidos (head xs) (x:xs) = head xs
                      | length (x:xs) > 2 && cuentoRepetidos x (x:xs) >= cuentoRepetidos (head xs) (x:xs) = masRepetidoAux (x:(tail xs))
                      | otherwise = masRepetidoAux ((head xs):(tail xs))

-- Devuelve una lista con todos los números de un tablero 
listaGeneral :: Tablero -> [Integer]
listaGeneral [] = []
listaGeneral (x:xs) = (x ++ listaGeneral xs)

-- Devuelve el valor más repetido de una sopa
masRepetido :: Tablero -> Integer
masRepetido (t:ts)  = masRepetidoAux (listaGeneral (t:ts))

-- Devuelve los números de los casilleros del camino
numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino (t:ts) [] = []
numerosDeCamino (t:ts) [x] = [valor(t:ts) x]
numerosDeCamino (t:ts) (x:xs) = valor (t:ts) x : numerosDeCamino (t:ts) xs

-- Devuelve True sí y solo sí algun elemento de la lista se repite
hayRepetidos :: [Integer] -> Bool
hayRepetidos [x] = False
hayRepetidos (x:xs) = elem x xs || hayRepetidos (xs)

-- Devuelve True sí y solo sí en un camino no aparecen números repetidos
caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos (t:ts) (x:xs) = not (hayRepetidos (numerosDeCamino (t:ts) (x:xs)))

-- Devuelve True sí y solo sí el camino es de Fibonacci
caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci (t:ts) [] = True
caminoDeFibonacci (t:ts) (x:xs) | length (x:xs) < 3 = True
								| length (x:xs) >= 3 && primerElemento + segundoElemento /= tercerElemento = False
								| otherwise = caminoDeFibonacci (t:ts) xs
				where 
					primerElemento = head(numerosDeCamino (t:ts) (x:xs))
					segundoElemento = head(tail(numerosDeCamino (t:ts) (x:xs)))
					tercerElemento = head(tail(tail(numerosDeCamino (t:ts) (x:xs))))

type Set a = [a]  

-- Devuelve una lista de tuplas que representan las coordenadas de una sopa
aCoordenadas :: Tablero -> (Integer, Integer) -> [(Integer, Integer)]
aCoordenadas [] (n, k) = []
aCoordenadas (t:ts) (n, k) | fromIntegral (length t) >= k = [(n, k)] ++ aCoordenadas (t:ts) (n, k+1)  
					 	   | fromIntegral (length t) < k = aCoordenadas (ts) (n+1, 1)

-- Filtra los caminos repetidos de una lista
filtro :: [[Integer]] -> [[Integer]]
filtro [] = []
filtro (x:xs) | elem x xs = filtro xs
			  | otherwise = x : filtro xs  
			  
-- Filtra los caminos válidos de una lista
caminoValido :: [(Integer, Integer)] -> Bool
caminoValido (x:[]) = True
caminoValido (x:y:ys) | (fst x == fst y  && snd x == snd y-1) || (snd x == snd y && fst x == fst y-1) = caminoValido (y:ys)
					  | otherwise = False

-- Filtra las listas de tuplas repetidas
filtroDeTuplas :: [[(Integer, Integer)]] -> [[(Integer, Integer)]]
filtroDeTuplas [] = []
filtroDeTuplas (x:xs) | elem x xs = filtroDeTuplas xs
			  		  | otherwise = x : filtroDeTuplas xs

-- Quita de la lista una tupla de la tupla indicada
quitarTupla :: (Integer, Integer) -> [(Integer,Integer)] -> [(Integer,Integer)]
quitarTupla n [] = []
quitarTupla n (x:xs) | n == x = xs
                     | otherwise = x : quitarTupla n xs

-- Indica la mayor tupla según la norma
maximaTupla :: [(Integer, Integer)] -> (Integer, Integer)
maximaTupla [(a,b)] = (a,b)
maximaTupla (x:y:ys) | fst x >= fst y && snd x >= snd y = maximaTupla (x : ys)
                     | otherwise = maximaTupla (y:ys) 

-- Ordena de menor a mayor una lista de tuplas según la norma
ordenarTupla :: [(Integer, Integer)] -> [(Integer, Integer)]
ordenarTupla [] = []
ordenarTupla xs = ordenarTupla (quitarTupla (maximaTupla xs) xs) ++ [maximaTupla xs]

--Busca y devuelve la tupla en la iesima posicion de la lista
iesimo :: [(Integer, Integer)] -> Integer -> (Integer, Integer)
iesimo (x:xs) i | i == 0 = x
				| otherwise = iesimo xs (i-1)

--Agrega una tupla a la lista de tuplas 				
agregar :: Integer -> [(Integer, Integer)] -> Set (Integer, Integer) -> Set (Integer, Integer)
agregar n xs ys | elem (iesimo xs (n-1)) ys || caminoValido (agregaTupla) == False = ys
                | otherwise = agregaTupla
            where agregaTupla = ordenarTupla((iesimo xs (n-1)) : ys)

-- Agrega una tupla indicada a la las listas de listas
agregarATodos :: Integer -> [(Integer, Integer)] -> [[(Integer, Integer)]] -> [[(Integer, Integer)]]
agregarATodos n xs [] = []
agregarATodos n xs (y:ys) = agregar n xs y : agregarATodos n xs ys

-- Devuelve las partes de una lista de coordenadas de longitud n
partes :: Integer -> [(Integer, Integer)] -> [[(Integer, Integer)]]
partes 0  xs = [[]]
partes n  xs = filtroDeTuplas(partesAnterior ++ agregarATodos n xs (partesAnterior))
	where partesAnterior = partes (n-1) xs
							                            
-- Filtra los caminos de fibonacci de una lista 
filtroDeFibonacci :: Tablero -> [Camino] -> [[Integer]]
filtroDeFibonacci (t:ts) [] = [] 
filtroDeFibonacci (t:ts) (x:xs) | caminoDeFibonacci (t:ts) x == True = [numerosDeCamino (t:ts) x] ++ filtroDeFibonacci (t:ts) xs
								| otherwise = filtroDeFibonacci (t:ts) xs							                            
							                            
-- Devuelve la lista de todos los caminos de fibonacci
todosLosCaminosDeFibonacci :: Tablero -> [[Integer]]
todosLosCaminosDeFibonacci (t:ts) = filtroDeFibonacci (t:ts) (partes cantidadElementos coordenadas)
                where 
                	cantidadElementos = cantidadFilas(t:ts) * cantidadColumnas(t:ts)
                	coordenadas = aCoordenadas (t:ts) (1,1)

-- Devuelve los números del camino más largo. Si hay más de uno con la misma longitud se queda con el primero
masLargo :: [[Integer]] -> [Integer]
masLargo (x:[]) = x
masLargo (x:y:ys) | length x >= length y = masLargo (x:ys)
				  | otherwise = masLargo (y:ys)

-- Devuelve los números del camino de fibonacci mas largo de una sopa
mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci (t:ts) = masLargo(todosLosCaminosDeFibonacci(t:ts))

-- Devuelve la lista de caminos de longitud k
caminosDeLongitudK :: Integer -> [[Integer]] -> [[Integer]]
caminosDeLongitudK k [] = []
caminosDeLongitudK k (x:xs) | fromIntegral (length x) == k = filtro(x : caminosDeLongitudK k xs) 
							| otherwise = caminosDeLongitudK k xs				
							
-- Devuelve la lista de caminos de longitud k de una sopa
secuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto [Integer]
secuenciasDeFibonacciDeLongitudK (t:ts) 0 = [[]]
secuenciasDeFibonacciDeLongitudK (t:ts) k = caminosDeLongitudK k (todosLosCaminosDeFibonacci(t:ts))
