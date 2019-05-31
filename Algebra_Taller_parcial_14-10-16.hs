-- Ej 1
-- kesimo, devuelvo el kesimo elemento de una lista
kesimo :: [Integer] -> Integer -> Integer
kesimo xs k | k == 1 = head xs
            | otherwise = kesimo (tail xs) (k-1)

-- Ej 2
-- domina devuelve True ssi cada elemento de xs es mayor al de ys en misma posicion
domina :: [Integer] -> [Integer] -> Bool
domina xs ys | length ys == 0 = False
             | length ys == 1 = dominaAux xs ys
             | length ys > 1 && ( dominaAux xs ys == False ) = False
             | length ys > 1 && ( dominaAux xs ys == True ) = domina (tail xs) (tail ys)

dominaAux :: [Integer] -> [Integer] -> Bool
dominaAux xs ys | (head xs) > (head ys) = True
                | otherwise = False

-- Ej 3
-- dada una lista de al menos 3 elementos, devuelve True si los 2 anteriores son iguales
-- a la suma del 3ero
esTipoAux :: [Integer] -> Bool
esTipoAux [] = True
esTipoAux [x] = True
esTipoAux [x,y] = False
esTipoAux xs | head xs + head (tail xs) /= head (tail(tail xs)) = False
             | head xs + head (tail xs) == head (tail(tail xs)) = True

esTipoFibonacci :: [Integer] -> Bool
esTipoFibonacci [] = esTipoAux []
esTipoFibonacci xs | length xs > 3 && head xs + head (tail xs) /= head(tail(tail xs)) = False
                   | length xs > 3 && head xs + head (tail xs) /= head(tail(tail xs)) = esTipoFibonacci (tail xs)
                   | otherwise = esTipoAux xs


-- Ej 4
