module Lista1 where

--------------- Exercício 1 ---------------

ehTriangulo a b c = if (a < b + c) &&
                    (b < a + c) &&
                    (c < a + b) then True else False


--------------- Exercício 2 ---------------

tipoTriangulo a b c = if (a == b && b == c) then "equilatero"

                    else if ((a == b || b == c || c == a) && (b /= a || b /= c || c /= a)) then "isosceles"

                    else "escaleno"

--------------- Exercício 3 ---------------

triangulo a b c = if ehTriangulo a b c then tipoTriangulo a b c else "nao eh um triangulo"

--------------- Exercício 4 ---------------

somaPares 0 = 0
somaPares 1 = 0
somaPares n =
    --Se n for PAR
    if (rem n 2 == 0) then n + somaPares(n - 2)

    --Se n for ÍMPAR, decresce um e volta pro caso par
    else somaPares(n - 1)

--------------- Exercício 5 ---------------

somaPot2m m 0 = m
somaPot2m m n = (2 ^ n) * m + somaPot2m m (n-1)

--------------- Exercício 6 ---------------

primo n
    | numDiv > 2 = False
    | otherwise = True
    where numDiv = length (divisores n)

divisores n = [x | x <- [1..n] , mod n x == 0]

--------------- Exercício 7 ---------------

seriePI n = seriePI' 1 1 n

seriePI' x i n = x * 4/i + if (4/i > 4/n) then seriePI' (negate x) (i+2) n else 0