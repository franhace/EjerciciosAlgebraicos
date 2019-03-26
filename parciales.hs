-- n e Fib??
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

esFib n = esFibAux n 1

esFibAux :: Integer -> Integer -> Bool
esFibAux n s | fib s == n = True
             | fib s > n = False
             | fib s < n = esFibAux n (s+1)

sumaFibo :: Integer -> Integer
sumaFibo 1 = (fib 0) + (fib 1)
sumaFibo n = (fib n) + (sumaFibo(n-1))

bisiesto n | mod n 400 == 0 = True
           | mod n 100 == 0 = False
           | mod n 4 == 0 = True
           | otherwise = False

agregar n xs | elem n xs = xs
            | otherwise = n:xs

funcionAn 0 = 0
funcionAn 1 = 1
funcionAn n | mod n 2 == 0 = div n 2
            | otherwise = n + 1

-- Lista
sumatoriaPares lista | length lista == 0 = 0
                     | ( head lista) `mod` 2 == 0  = head lista + sumatoriaPares ( tail lista)
                     | otherwise = sumatoriaPares ( tail lista)

mezcla :: [Integer] -> [Integer] -> [Integer]
mezcla xs ys | length xs == 0 = ys
             | length ys == 0 = xs
             | head xs <= head ys = ( head xs) : (mezcla (tail xs) ys)
             | otherwise = (head ys) : ( mezcla (tail ys) xs)

-- filtro : [Integer] -> Integer -> [Integer]
filtro xs x | length xs == 0 = []
           | head xs == x = filtro (tail xs) x
           | head xs /= x = (head xs): filtro (tail xs) x

-- posiciones :: integer -> [integer] -> [integer]
juntarIndices indice elemento lista
  | length lista == 0 = []
  | head lista == elemento = indice : juntarIndices (indice + 1) elemento (tail lista)
  | head lista /= elemento = juntarIndices (indice + 1) elemento (tail lista)
posiciones elemento lista = juntarIndices 1 elemento lista

-- inserta y ordena por insercion
-- inserta :: integer -> [integer] -> [integer]
inserta elemento lista | length lista == 0 = elemento:lista
                     | elemento > (head lista) = (head lista) : (inserta elemento (tail lista))
                     | otherwise = elemento : lista
-- orden :: [integer] -> [integer]
orden lista | length lista == 0 = []
            | otherwise = inserta (head lista) (orden (tail lista))
