menorLex :: (Float, Float, Float) -> (Float,Float,Float) -> Bool
menorLex (a,b,c) (x,y,z) = (a < x) || (a == x && b < y) || (a == x && b == y && c < z)


fibN :: Integer -> Integer
fibN 0 = 1
fibN 1 = 1
fibN n = fibN (n-1) + fibN (n-2)


sumaFibonacci :: Integer -> Integer
sumaFibonacci 0 = fibN 0
sumaFibonacci n = fibN n + sumaFibonacci (n-1)

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n 1
  | mod n 1 == 0 = 1
sumaDivisoresHasta n k
  | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
  | otherwise = sumaDivisoresHasta n (k-1)

esDefectivo :: Integer -> Bool
esDefectivo 1 = True
esDefectivo n = sumaDivisoresHasta n (n-1) < n

maximaDistancia :: [Integer] -> Integer
maximaDistancia [a,b] = abs (a - b)
maximaDistancia (x1:x2:xs)
  | dist > maximaDistancia (x2:xs) = dist
  | otherwise = maximaDistancia (x2:xs)
    where dist = abs (x1 - x2)


traducir :: [Integer] -> [(Integer,Integer)]
traducir [] = []
traducir (x:xs) = (x,1) : traducir xs

comprimir :: [Integer] -> [(Integer,Integer)]
comprimir xs = comprimirAux (traducir xs)

comprimirAux :: [(Integer, Integer)] -> [(Integer,Integer)]
comprimirAux [x] = [x]
comprimirAux ((a,b):(c,d):xs)
  | (a == c) = comprimirAux ((a,b+d):xs)
  | otherwise = ((a,b):comprimirAux ((c,d):xs))
