f n | n /= 0 = 0
    | n == 0 = 1


g n | n == 0 = 1
    | otherwise = 0

signo n | n > 0 = 1
	| n == 0 = 0
	| otherwise = -1

absoluto x | x >= 0 = x
           | otherwise = -1 * x


maximo x y | x > y = x
           | otherwise = y

maximo3 x y z = max ((max x y)) (z)

triple x = (x * 3)

normaVectorial p = sqrt ((fst p)^2 + (snd p)^2)


esMultiploDe x y = mod x y == 0
alMenosMultiploDe x y n = (esMultiploDe x n) || (esMultiploDe y n)
