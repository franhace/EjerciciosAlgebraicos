sumatoriaPares :: [Integer] -> Integer
sumatoriaPares [] = 0
sumatoriaPares (x:xs) | esPar x == True = x + sumatoriaPares xs
                      | otherwise = sumatoriaPares xs

esPar :: Integer -> Bool
esPar x | mod x 2 == 0 = True
        | otherwise = False

quitarTodas :: [Integer] -> Integer -> [Integer]
quitarTodas [] _ = []
quitarTodas (x:xs) n | x == n = quitarTodas xs n
                     | otherwise = x : quitarTodas xs n

inserta :: Integer -> [Integer] -> [Integer]
inserta n [] = [n]
inserta n [x] | n > x = [x,n]
              | otherwise = [n,x]
inserta n (x:xs) | n <= x = n:x:xs
                 | otherwise = x : inserta n xs

ordena :: [Integer] -> [Integer]
ordena [x,j] | x < j = [x,j]
             | otherwise = [j,x]
ordena (x:j:xs) | x > j = j : (ordena (x:xs) )
                | otherwise = x : (ordena (j:xs))
ordenaPorInsercion :: [Integer] -> [Integer]
ordenaPorInsercion xs
    | length xs == 0 = []
    | otherwise = inserta (head xs) (ordenaPorInsercion (tail xs))