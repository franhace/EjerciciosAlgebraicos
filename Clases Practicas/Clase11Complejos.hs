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

-- suma 2 polinomios
suma :: Polinomio -> Polinomio -> Polinomio
suma [] s = s
suma p [] = p
suma (ps) (ss) = limpiar (sumaAux ps as)

sumaAux :: Polinomio -> Polinomio -> Polinomio
sumaAux [] s = s
sumaAux p [] = p
sumaAux (ps) (ss) 
	| grado (ps) == grado (ss) = (((head ps) + (head ss)) : sumaAux ps ss)
	| grado (p:ps) > grado (s:ss) = head ps : sumaAux (tail ps) (ss)
	| otherwise = head ss : sumaAux ps (tail ss)

limpiar :: [Float] -> Polinomio
limpiar [] = []
limpiar (p:ps) | p == 0 = limpiar ps
			   | otherwise = (a:as)

-- devuelve la diferencia entre polinomios
diferencia :: Polinomio -> Polinomio -> Polinomio
diferencia (ps) (ss) | length (ps) == length (ss) = []
diferencia (ps) (ss) 
	| length (ps) > length (ss) = head ps : diferencia (tail xs) ps
	| otherwise = head ss : diferencia ps (tail ss)

-- calcula el producto escalar por un polinomio
productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 _ = []
productoPorEscalar _ [] = []
productoPorEscalar n [x] = n * x
productoPorEscalar n (x:xs) = n * x : productoPorEscalar n xs

-- calcula el producto por un monomio aX^n
-- ax^n -> (a,n)
productoPorMonomio :: (Float, Integer) -> Polinomio -> Polinomio
productoPorMonomio (a,n) p = (productoPorEscalar a p ) ++ ceros n

ceros :: Integer -> [Float]
ceros n 
	| n == 0 = []
	| otherwise = 0:ceros (n-1)


-- calcular el producto de dos polinomios
-- gr(p*q) = gr p + gr q
producto :: Polinomio -> Polinomio -> Polinomio
producto p [] = p
producto [] s = s
producto [x] ss = productoPorEscalar x ss
producto ps ss = productoPorEscalar (head ps) ss : producto (tail xs) ss 
producto x:xs ys = suma ( productoPorMonomio (x, grado (x:xs)) ys ) (producto xs ys)


[x] _ = (x,0)
sumaComplejos (productoComplejos (x,0))

productoAux :: Polinomio -> Polinomio -> Polinomio
productoAux ps [] = ps
productoAux [] ss = ss
productoAux ps ss 
	| grado ps == grado ss = (head ps) * (head ss) : productoAux (tail ps) (tail ss)
	| grado ps > grado ss = head ps : productoAux (tail ps) ss
	| otherwise = head ss : productoAux ps (tail ss)

[2 0 0 1] [0 2 2 1] = 

type Complejo = (Float, Float)

sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (a,bi) (c,di) = (a+c, bi+di)

-- producto (a,ai) (b,bi) = (a*c + a*di + bi*c + bi*di)
productoComplejos :: Complejo -> Complejo -> Complejo
productoComplejos (a,bi) (c,di) = (a*c - b*d, a*di + bi*c)
                       where b = bi
                             d = di

-- evalúe un polinomio (con coeficientes reales) en un número complejo.
evaluarComplejo :: Polinomio -> Complejo -> Complejo
evaluarComplejo [] (a,bi) = 



























