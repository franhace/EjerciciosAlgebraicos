-- Reutilizar codigo
-- module ....
-- si se especifican las funciones solo se exportan esas
-- where 


module Clase3
where


-- Ejercicios de enteros
signo2 :: Int -> Int
signo2 x | x >= 0 = 1
         | otherwise = (-1)

-- Devuelve las unidades de un entero

unidades :: Int -> Int
unidades x = (signo2 x) * (mod (abs x) 10)

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

-- Relaciones
-- r1 si a y b = paridad
r1 :: Int -> Int -> Bool
r1 a b = mod a 2 == mod b 2

-- r2 si 2a + 3b div por 5
r2 :: Int -> Int -> Bool
r2 a b = mod (a * 2 + b * 3 ) 5 == 0 

-- r3 si digitos de unidades de a, b y a x b son iguales
r3 :: Int -> Int -> Bool
r3 a b = unidades a /= unidades b && unidades a /= unidades (a*b)

-- Rel Equivalencia (-inf, 3) U (3, inf)
rel3 :: Float -> Float -> Bool
rel3 x y | x < 3 && y < 3 = True
	 | x > 3 && y > 3 = True
	 | otherwise = False

-- Rel Equivalencia (-inf, 3) U [3, 7) U [7, inf)
rel37 :: Float -> Float -> Bool
rel37 x y | x < 3 && y < 3 = True
	  | (x == 3 && x < 7) && (y == 3 && y < 7) = True
	  | x >= 7 && y >= 7 = True
	  | otherwise = False

-- 10. sonTuplasMultiplo
-- 1 k pertenece a Enteros
--sonTuplasMultiplo :: ( Int, Int ) -> ( Int, Int ) -> Bool
-- sonTuplasMultiplo ab pq | a /= 0 && b /= 0 = ( div p a == div q b ) && ( mod p a == mod q b)
--			| a == 0 && b /= 0 = p == 0 && mod q b = 1
--			| a /= 0 && b == 0 = q == 0 && mod p a = 0

-- 2 k pertenece a Reales, como el k no es parte de la signatura de la funcion, 
-- mantenemos el int y no ponemos Float
--sonTuplasMultiplo :: ( Int, Int ) -> ( Int, Int ) -> Bool
-- sonTuplasMultiplo ab pq = b * p == a * q


-- 11. Dados (a, b) y (p, q), determinar el tipo e implementar funciones
--     que determinen si (a, b) esta relacionado con (p, q) cuando:
-- 11.1: (a, b) = k*(p, q), con a,b,p,q,k Reales, sin el cero
--'(a, b) == k*(p, q)' es lo mismo que
-- 'a == k*p' y 'b == k*q'?
-- 'a/p == k' y 'b/q == k'?
-- 'a/p == b/q'?
rela111 :: (Float, Float) -> (Float, Float) -> Bool
rela111 ab pq = ((fst ab) / (fst pq)) == ((snd ab) / (snd pq))
-- 11.2: lo mismo que 11.1, pero con a,b,p,q Enteros, y k Real, sin el cero
-- '(a, b) == k*(p, q)' es lo mismo que
-- 'a/p == b/q', pero como son Integers y la division no esta definida,
-- 'pasamos' multiplicando los denominadores y obtenemos
-- 'a*q == b*p'
rela112 :: (Float, Float) -> (Float, Float) -> Bool
rela112 ab pq = ((fst ab) * (snd pq)) == ((snd ab) * (fst pq))
--11.3: Opcional: lo mismo que 11.2 pero incluyen el (0,0):
rela113 :: (Float, Float) -> (Float, Float) -> Bool
rela113 ab pq | (fst pq == 0) || (snd pq == 0) = False
              | otherwise = rela112 ab pq

-- Factorial 
factorial :: Integer -> Integer
factorial n | n == 1 = 1
	    | otherwise = n * factorial (n-1)

-- sc 
sc :: Integer -> Integer 
sc n | n == 0 = 0
     | otherwise = sc(n-1) + n ^ 2

-- fibonacci, devuelve el i-esimo termino de la serie
fib :: Integer -> Integer 
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib (n-2)


-- Sucesiones, devuelve el n-esimo termino de la suc
suc1 :: Integer -> Integer 
suc1 n | n == 0 = 2
       | otherwise = 2 * n * suc1 (n - 1) + ((2 ^ (n+1)) * factorial n)
-- aca fue necesario usar el ^ en vez de **

 
suc2 :: Integer -> Integer
suc2  n | n == 1 = 1
        | n == 2 = 2
        | otherwise = n * suc2 ( n-1 ) + 2 * (n+1) * suc2 ( n-2)

-- suc3 :: Integer -> Integer
-- suc3 n | n == 1 = -3
--       | n == 2 = 6
--       | esPar n = suc3 ( n-1 ) + 2 * suc3 (n-2) + 9
--       | otherwise = ((suc3 (n-1)) * (-1)) -3






























