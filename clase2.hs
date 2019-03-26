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
