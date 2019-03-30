f n | n /= 0 = 0
    | n == 0 = 1


g n | n == 0 = 1
    | otherwise = 0

-- signo, devuelve 1 si n es > 0, 0 si es = 0 o -1 si es < 0
signo n | n > 0 = 1
	| n == 0 = 0
	| otherwise = -1

-- fx que devuelve el absluto
absoluto x | x >= 0 = x
           | otherwise = -1 * x

-- fx que devuelve el max entre 2 numeros
maximo x y | x > y = x
           | otherwise = y  	

-- absolutobis x | (maximo x x*-1 ) 

-- fx que devuelve el max entre 3 numeros
maximo3 x y z = max ((max x y)) (z)


-- devuelve un numero multiplicado por 3
triple x = (x * 3)

-- devuelve la norma de un vector en R2
normaVectorial p = sqrt ((fst p)^2 + (snd p)^2)

-- se fija si x es multiplo de y
esMultiploDe x y = mod x y == 0

-- se fija si hay al menos un multiplo
alMenosMultiploDe x y n = (esMultiploDe x n) || (esMultiploDe y n)

-- doble x 
doble :: Int -> Int
doble x = ( x * 2 )

--esPar
esPar:: Integer -> Bool
esPar num	| (mod num 2 == 0) = True
		| otherwise = False

esMulti:: Integer -> Integer -> Bool
esMulti num den | (mod num den == 0) = True
		|otherwise = False


-- Notacion infija -- 
-- 11 `mod` 2
-- = 1
-- (+) 2 3
-- = 5

crearPar:: a -> b -> (a,b)
crearPar x y = (x,y)

invertir:: (a,b) ->(b,a)
invertir x = (snd(x),fst(x))

distanciaPuntos:: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos puntoP puntoQ = sqrt((fst(puntoQ)-fst(puntoP))^2 + (snd(puntoP) -snd(puntoQ))^2)

--Ej 32 Practica 1

ej32F1:: (Float) -> (Float, Float, Float)
ej32F1 x = (2*x,x^2,x-7)

ej32F2:: (Integer) -> (Integer)
ej32F2 n	|(mod n 2 == 0) = div n 2
		|otherwise = n + 1


--Ej 33 P1
f:: (Int) -> (Int)
f n |(mod n 6 == 0) = (div (n^2) 2)
	|otherwise = 3*n + 1

g:: (Int,Int) -> (Int)
g (n, m) = n*(m+1)




