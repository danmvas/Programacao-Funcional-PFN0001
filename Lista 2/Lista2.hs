module Lista2 where

--ex1
pertence a [] = False
pertence a (x : xs)
  | x == a = True
  | otherwise = pertence a xs

--ex2
intercessao xs ys = [x | x <- xs, y <- ys, x == y]

--ex3
inversoLista [] = []
inverso (x : xs) = inversoLista xs ++ [xs]

--ex4
nUltimos n [] = []
nUltimos n xs
  | n == 0 = []
  | otherwise = elem : nUltimos (n -1) list
  where
    elem = last xs
    list = init xs

--ex6
-- repete x n vezes
repetir n m
  | n == 0 = []
  | otherwise = m : repetir (n - 1) m

--ex15
separar1 v (x:xs) = [a | a <- (x:xs), a <= v]
separar2 v (x:xs) = [a | a <- (x:xs), a > v]
separarDuplas v (x:xs) = (separar1 v (x:xs), separar2 v (x:xs))

--ex18
simetrico [] = []
simetrico (x:xs) | fst x == snd x = True : simetrico xs
                 | otherwise = False : simetrico xs

--ex22
repeteOutro n [] = []
repeteOutro n (x : xs) = repetir n x ++ repeteOutro (n -1) xs

magica xs = lista ++ nUltimos (length lista -1) (inversoLista lista)
  where lista = repeteOutro (length xs) xs