milista = [2,3,1,12,1,2,1,21]

-- ultimo
elUltimo :: [Integer] -> Integer
elUltimo [x] = x
elUltimo (x:xs) = elUltimo xs

-- anteultimo
anteultimo :: [Integer] -> Integer
anteultimo [x, j] = x
anteultimo (x:xs) = anteultimo xs

-- kesimo
kesimo :: [Integer] -> Integer -> Integer
kesimo xs k | k == 1 = head xs
            | otherwise = kesimo (tail xs) (k-1)

-- numero elementos
numElem :: [Integer] -> Integer
numElem [] = 0
numElem [x] = 1
numElem xs = 1 + numElem (tail xs)

-- reversar
reversar :: [Integer] -> [Integer]
reversar [] = []
reversar [x] = [x]
reversar [x,j] = [j,x]
reversar (xs) = (elUltimo xs:(reversar (init xs)))

-- es palindromo?
palindromo :: [Integer] -> Bool
palindromo [] = True
palindromo [x] = True
palindromo [x,j] = x == j
palindromo xs | (head xs) /= (elUltimo xs) = False
              | otherwise = palindromo (init(tail xs))

-- elimina consecutivos duplicados
cd :: [Integer] -> [Integer]
cd [] = []
cd [x] = [x]
cd [x,j] | x == j = [x]
cd (x:xs) | x == head xs = cd(x:tail xs)
          | otherwise = (x:cd xs)

-- dice cuantos consecutivos hay
-- tuplamos la lista
tuplar :: [Integer] -> [(Integer, Integer)]
tuplar [] = []
tuplar (x:xs) = (x,1):tuplar xs

-- juntamos las tuplas
juntarTuplas :: [(Integer,Integer)] -> [(Integer,Integer)]
juntarTuplas [x] = [x]
juntarTuplas ((a,b):(x,y):xs) | (a == x) = juntarTuplas ((a,b+y):xs)
                              | otherwise = (a,b):juntarTuplas ((x,y):xs)

comprimir :: [Integer] -> [(Integer, Integer)]
comprimir xs = juntarTuplas (tuplar xs)


-- duplica elementos
duplicador :: [Integer] -> [Integer]
duplicador [] = []
duplicador (x:xs) = x : x : duplicador xs

-- repite n veces un elemento
repetirNveces :: Integer -> Integer -> [Integer]
repetirNveces _ 0 = []
repetirNveces x n = x : repetirNveces x (n - 1)

-- repite n veces cada elem de una lista

nplica :: [Integer] -> Integer -> [Integer]
nplica [] _ = []
nplica [x] n = repetirNveces x n
nplica (x:xs) n = repetirNveces x n ++ nplica xs n

-- repite cada numero (Indice en la lista) veces
test :: [a] -> [a]
test = test' 1
    where test' _ [] = []
          test' i (x:xs) = rep i
              where rep j | j > 0 = x : rep (j-1)
                          | otherwise = test' (i+1) xs

-- elimina elementos de una lista cada n veces
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x : helper xs (k-1)

-- elimina elem en la posicion K
goodDeleteN :: Int -> [a] -> [a]
goodDeleteN _ []     = []
goodDeleteN i (a:as)
  | i == 1    = as
  | otherwise = a : goodDeleteN (i-1) as