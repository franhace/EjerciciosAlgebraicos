typle Polinomio = [Float]

-- Devuelve el grado de un polinomio
grado :: Polinomio -> Integer
grado [x] = 0
grado (x:xs) = 1 + grado xs
grado [] = undefined

-- Dado un polinomio , calcula su valor para un x dado
evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (a:as) x = a * (f ^(grado(a:as)) ) + evaluar as x

-- calcula el polinomio derivado
derivada :: Polinomio -> Polinomio
derivada [] = []
derivada [x] = []
derivada (x:xs) = fromInteger x * grado (x:xs) : derivada xs

-- calcula el polinomio derivado n veces
derivadaN :: Integer -> Polinomio -> Polinomio 
derivadaN 1 xs = derivada xs
derivadaN n xs = derivada (derivada(n-1) xs)

suma :: Polinomio -> Polinomio -> Polinomio
suma 