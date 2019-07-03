-- ej 1
mayorDistancia :: (Integer, Integer, Integer) -> Integer
mayorDistancia (a,b,c)
    | ((abs (a - b)) > (abs (b-c))) && ((abs (a - b)) > (abs (a-c))) = abs(a - b)
    | ((abs (a - c)) > (abs (a-b))) && ((abs (a - c)) > (abs (b-c))) = abs(a - c)
    | otherwise = abs(b - c)

-- ej 2
sumaAux :: Integer -> Integer
sumaAux 1 = 3
sumaAux n = n^2 + 2*n + sumaAux (n-1)

suma :: Integer -> Integer
suma n = sumaAux (2*n)

-- ej 3
esPar :: Integer -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False

cambioParidad :: Integer -> Integer -> Bool
cambioParidad u v = esPar u /= esPar v

-- Fx que cuenta cuantos cambios de signo hay
cantCambiosParidad :: [Integer] -> Integer
cantCambiosParidad [] = 0
cantCambiosParidad [x] = 0
cantCambiosParidad [x,j]
    | cambioParidad x j == True = 1
	| otherwise = 0
cantCambiosParidad (x:j:xs)
    | cambioParidad x j == True = 1 + cantCambiosParidad (j:xs)
    | otherwise = cantCambiosParidad (j:xs)