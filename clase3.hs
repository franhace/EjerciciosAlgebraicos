-- Ejercicios de enteros
signo :: Int -> Int
signo x | x >= 0 = 1
        | otherwise = (-1)

-- Devuelve las unidades de un entero
unidades :: Int -> Int
unidades x = (signo x) * (mod (abs x) 10)

unidadesbis :: Int -> Int
unidadesbis x = rem x 10

-- Suma digitos de unidades de 3 enteros
sumaUnidades3 :: Int -> Int -> Int -> Int
sumaUnidades3 x y z = unidades x + unidades y + unidades z

-- Determina si 3 numeros enteros son impares
todosImpares :: Int -> Int -> Int -> Bool
todosImpares x y z = ((mod x 2) /= 0) && ((mod y 2) /= 0) && ((mod z 2) /= 0)

-- Determina si hay al menos 1 impar 
alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar x y z = ((mod x 2) /= 0) || ((mod y 2) /= 0) || ((mod z 2) /= 0)

-- Determina si hay al menos 1 impares
alMenosUnImpar2 :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar2 x y z | ((mod x 2) /= 0) || ((mod y 2) /= 0) = True 
		      | ((mod z 2) /= 0) = True
		      | otherwise = False

-- Determina si hay al menos 2 impares
alMenosDosImpares :: Int -> Int -> Int -> Bool
alMenosDosImpares x y z = (mod x 2) + (mod y 2) + (mod z 2) >= 2 

-- Determina si hay 2 pares
alMenosDosPares :: Int -> Int -> Int -> Bool
alMenosDosPares x y z | alMenosDosImpares x y z == True = False
		      | otherwise = True 

alMenosDosPares2 :: Int -> Int -> Int -> Bool
alMenosDosPares2 x y z = not (alMenosDosImpares x y z)

-- Determina si dados 3 numeros enteros, algunos de los 2 primeros es multiplo del 3ero
alMenosUnMultiploDe :: Int -> Int-> Int-> Bool
alMenosUnMultiploDe x y z = (mod x z == 0) || (mod y z == 0) 
