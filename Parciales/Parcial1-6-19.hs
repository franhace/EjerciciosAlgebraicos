

-- Ej 3
-- devuelve los digitos de un integer, separados en una lista
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

esCapicuaAux :: [Integer] -> Bool
esCapicuaAux (xs)
    | length xs <= 1 = True
    | head xs /= last xs = False
    | otherwise = esCapicuaAux (init (tail xs))

esCapicua :: Integer -> Bool
esCapicua a = esCapicuaAux (digs a)


-- Ej 4


-- Ej 5

suce :: Integer -> Integer
suce 1 = 1
suce 2 = 2
suce 3 = 5
suce n = 3*(suce (n-1))^2 + 2*(suce(n-2)) + suce (n-3)

listarSuce :: Integer -> [Integer]
listarSuce n = [suce n] ++ listarSuce (suce (n+1))

esTerminoDeSuc :: Integer -> Integer -> Bool
esTerminoDeSuc n m
    | (suce m) == n = True
    | (suce m ) > n = False
    | (suce m) < n = esTerminoDeSuc n (m+1)

listaConTerminosSuc :: [Integer] -> [Integer]
listaConTerminosSuc [] = []
listaConTerminosSuc (x:xs)
    | (esTerminoDeSuc x 1 == True) = x : listaConTerminosSuc xs
    | otherwise = listaConTerminosSuc xs

sumaTerminos :: [Integer] -> Float
sumaTerminos [] = 0
sumaTerminos (x:xs) = fromInteger x + sumaTerminos xs