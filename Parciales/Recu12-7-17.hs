
-- Ej 1
f :: Integer -> Integer
f n | mod n 5 == 0 = n ^ 2
    | otherwise = n - 1

g :: Integer -> (Integer, Integer)
g n = (n * 3, n ^ 3)

h :: Integer -> (Integer, Integer)
h n = g (f n)

-- Ej 2
-- funcion que devuelve true si todos los digitos de un
-- numero son impares
esPar :: Integer -> Bool
esPar n = mod n 2 == 0

digitos :: Integer -> [Integer]
digitos 0 = []
digitos x = digitos (x `div` 10) ++ [x `mod` 10]

listaImpares :: [Integer] -> [Integer]
listaImpares [] = []
listaImpares [x] | esPar x == False = [x]
                 | otherwise = []
listaImpares (x:xs) | esPar (x) == True = listaImpares xs
                    | otherwise = x: listaImpares xs

todosImpares :: Integer -> Bool
todosImpares n | (length (digitos n)) == (length (listaImpares (digitos n))) = True
               | otherwise = False

-- Ej 3
suma :: Integer -> Integer
suma 1 = 1
suma n = n ^ 3 + suma (n-1)

productoria :: Integer -> Integer
productoria 1 = 3
productoria m = (m^2 + 2*m)*(productoria (m-1))

s :: Integer -> Integer -> Integer
s n m = suma n + productoria m

-- Devuelve elementos en la posicon par
pos_par :: [a] -> [a]
pos_par [] = []
pos_par [x] = [x]
pos_par (x1:_:t) = x1 : pos_par t

-- Devuelve elementos en la posicon impar
pos_impar :: [a] -> [a]
pos_impar [] = []
pos_impar [x] = []
pos_impar [x,j] = [j]
pos_impar (x1:x2:t) = x2 : pos_impar t

-- Duplica elem en pos par o impar
dupeven :: [a] -> [a]
dupeven [] = []
dupeven (x:xs) = x : x : dupodd xs

dupodd :: [a] -> [a]
dupodd [] = []
dupodd (x:xs) = x : dupeven xs

squareeven :: [Integer] -> [Integer]
squareeven [] = []
squareeven (x:xs) | esPar x == True = (x^2):squareeven xs
                  | otherwise = x:squareeven xs