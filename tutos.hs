-- Tutorial

divisible x y = if (x `mod` y) == 0
    then "son divisibles"
    else "no son divisibles"

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

factorial 0 = 1
factorial n = factorial(n-1)*n

NumsImparesMen20 = [x | x <- [1..20] , x `mod` 2 == 1]

sc 0 = 0
sc n = sc (n-1) + (n * n)

scc1 0 = 2
scc1 n = (2 * n * scc1 (n-1)) + ((2 ** (n+1)) * factorial n)

scc2 0 = 1
scc2 1 = 2
scc2 n = (n * (scc2 (n-1))) + 2 * ((n+1) * scc2 (n-2))

scc3 0 = (-3)
scc3 1 = 6
scc3 n | rem n 2 == 0 = scc3 (n-1) + 2 * scc3 (n-2) + 9
       | otherwise = (-1) * ((scc3 (n-1)) + 3)

-----

guarda x | (x >= 0) = x
         | (x <= 0) = (-x)
         | otherwise = 3

andy :: Bool -> Bool -> Bool
andy False _ = False
andy _ False = False
andy True True = True

nomes = ("lucas", "juan", "marco")

--

normaVectorial p = sqrt ((fst p)^2 + (snd p)^2)

maximo x y | x > y = x
           | otherwise = y
maximo3 x y z = max ((max x y)) (z)

esPar n | mod n 2 == 0 = True
        | otherwise = False
esMultiploDe x y | mod y x == 0 = True
                 | otherwise = False

crearPar a b = (a, b)

funcion1 n = (2*n, n**2, n-7)
funcion2 n | esPar n = div n 2
           | otherwise = n+1

funcion6 n | mod n 6 == 0 = div (n * n) 2
           | otherwise = 3 * n + 1
funcion7 p = (fst p) * ((snd p) + 1)
funcion8 p = funcion6 (funcion7 p)

---

alMenosUnMultiploDe x y z = (esMultiploDe x z ) || (esMultiploDe x y)

todosImpares a b c = mod a 2 == 1 && mod b 2 == 1 && mod c 2 == 1
alMenosUnImpar a b c | mod a 2 == 1 = True
                   | mod b 2 == 1 = True
                   | mod c 2 == 1 = True
                   | otherwise = False

alMenos2impares a b c = (mod a 2) + (mod b 2) + (mod c 2) >= 2
alMenos2pares a b c = not (alMenos2impares a b c)
alMenosUnMultiploDe2 a b c = ((rem c a) == 0) || ((rem c b) == 0)

signo x | x >= 0 = 1
        | otherwise = (-1)
unidades x = (signo x) * (mod (abs x) 10)
uni x = rem x 10
sumaUnidades a b c = (uni a) + (uni b) + (uni c)


-- Enteros Relaciones

r1 a b = mod a 2 == mod b 2
r2 a b = mod ((2 * a) + (5 * b) ) 5 == 0
divisible5bis a b = (rem((2 * a) + (5 * b) ) 5) == 0
r3 a b = uni a /= uni b

rel1 ab pq = ((fst ab) / (fst pq)) == ((snd ab) / (snd pq))
rel2 ab pq = ((fst ab) * (snd pq)) == ((snd ab) * (fst pq))
rel3 ab pq | ((fst pq) == 0 || (snd pq) == 0) = False
           | otherwise = (rel2 ab pq)

-- Recursion

sumatoria n | n == 0 = 0
            | otherwise = n + sumatoria (n-1)

sumatoria1 n | n == 0 = 1
             | otherwise = (2 ** n) + sumatoria1 (n-1)

sumatoria2 n q | n == 1 = q
               | otherwise = (q ** n) + (sumatoria2 (n-1) q)

sumatoria3bis n q = sumatoria2 (2 * n) q

sumatoria4 n q = sumatoria3bis n q - sumatoria2 n q

esPar3 n | n == 0 = True
        | n < 2 = False
        | otherwise = esPar (n-2)

esPar4 n | n == 0 = True
         | otherwise = not (esPar4 (n-1))

esNatural3 n | n == 0 = True
             | n < 3 = False
             | otherwise = esNatural3 (n-3)

sumaImpares n | n == 1 = 1
              | otherwise = 2 * (n-1) + sumaImpares (n-1)

sumaLosPrimerosNImpares n | n == 1 = 1
                          | n > 1 = n_esimoImpar + sumaLosPrimerosNImpares (n-1)
                          where n_esimoImpar = 2 * (n - 1)
--
induc1 n | n == 1 = 1
         | otherwise = (2*n-1) + induc1(n-1)
fact1 n | n == 0 = 1
        | otherwise = n * fact1(n-1)

eAprox n | n == 0 = 1
         | otherwise = (1/fromInteger(fact1(n))) + eAprox (n-1)

parteEntera n | n < 1 = 0
              | otherwise = 1 + parteEntera(n-1)
parteEnteraNeg n | n >= 0 && n < (1) = 0
                 | otherwise = signo(n)*1 + parteEntera (n+((-1)*signo(n)))
--Algo div
division a d | a < d || a < (signo (d)) = (0, a)
              | otherwise = (fst qr' + 1, snd qr')
              where qr' = division (a-d) d
--extender a negativos: TAREA

sumaDivHasta n k | k == 1 = 1
                  | mod n k == 0 = k + recur
                  | otherwise = recur
                  where recur = sumaDivHasta n (k-1)
sumDiv n = sumaDivHasta n n

menosDivisorDesde n k | n-k == 0 = k
                      | otherwise = menosDivisorDesde n (k+1)
expo1 n m i j | i == 1 = 1
              | j == 1 = 1
              | otherwise = i^j + expo1 n m i (j-1)

-- Pattern Matching

-- final
type Set a = [a]

mul7 (x:xs) | mod x 7 == 0 = x:mul7(xs)
            | otherwise = mul7(xs)

agregar n xs | elem n xs = xs
              | otherwise = n:xs
