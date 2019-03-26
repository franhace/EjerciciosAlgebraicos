
-- Devuelve el numero en k posicion de una lista
kesimo :: [Integer] -> Integer -> Integer
kesimo xs k | k == 1 = head xs
            | otherwise = kesimo (tail xs)(k-1)

-- Dice si todos los numeros de una lista posicion a posicion son mayores que los de la otra lista
--dominaaux :: [integer] -> [integer] -> Bool
dominaaux xs ys | head xs > head ys = True
                 | head xs <= head ys = False
--domina :: [integer] -> [integer] -> Bool
domina xs ys | length ys == 0 = False
             | length ys == 1 = dominaaux xs ys
             | length ys > 1 && head xs <= head ys = False
             | length ys > 1 && head xs > head ys = domina (tail xs)(tail ys)

-- esTipoFibonacci :: [integer] -> Bool
esTipoFibonacci xs | length xs == 3 = esTipoFibonacciAux xs
                   | length xs > 3 && head xs + head (tail xs) /= head (tail (tail xs)) = False
                   | length xs > 3 && head xs + head (tail xs) == head (tail (tail xs)) = esTipoFibonacci ( tail xs )

-- esTipoFibonacciAux :: [integer] -> Bool
esTipoFibonacciAux xs | head xs + head (tail xs) /= head(tail(tail xs)) = False
                      | head xs + head (tail xs) == head(tail(tail xs)) = True

-- todosIguales :: [integer] -> Bool
todosIguales [] = True
todosIguales (xs:[]) = True
todosIguales xs | head xs /= head (tail xs) = False
                | otherwise = todosIguales (tail xs)


sacarTodos [] _ = []
sacarTodos xs [] = xs
sacarTodos xs (n:ns) = sacarTodos (sacar n xs) ns
sacar _ [] = []
sacar n (x:xs) | n == x = sacar n xs
               | otherwise = x:(sacar n xs)
