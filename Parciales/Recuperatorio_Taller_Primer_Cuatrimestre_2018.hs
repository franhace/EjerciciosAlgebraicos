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
esCuadrado :: Integer -> Integer -> Bool
esCuadrado n m | n == m^2 = True
                | otherwise = False

-- kesimo
kesimo :: [Integer] -> Integer -> Integer
kesimo xs k | k == 1 = head xs
            | otherwise = kesimo (tail xs) (k-1)

lugarCuadrado :: [Integer] -> Integer

lugarCuadrado xs
    | length xs < 1 = 0
    | length xs == 1 =
    | esCuadrado (kesimo xs (indice+1)) (indice+1) == True = 1 + lugarCuadrado (tail xs)
    | otherwise = lugarCuadrado (tail xs)
        where indice = 0
