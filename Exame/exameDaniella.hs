import Data.List
import Data.Function

-----------------------------------------------------------------------------------------

-- Nome: Daniella Martins Vasconcellos
-- Exame 20.04.2021
-- PFN 2020.2

-----------------------------------------------------------------------------------------
--funções a serem usadas pelas questões--

separaMenor :: Ord a => [a] -> a -> [a]
separaMenor (x : xs) v = [a | a <- (x : xs), a < v]

separaMaior :: Ord a => [a] -> a -> [a]
separaMaior (x : xs) v = [a | a <- (x : xs), a > v]

confereMaior :: Ord a => [(t, a)] -> (t, a)
confereMaior (x:xs) = maiorAtual x xs
  where maiorAtual maiorNum [] = maiorNum
        maiorAtual (y, z) (x:xs)
          | z < (snd x) = maiorAtual x xs
          | otherwise   = maiorAtual (y, z) xs

-----------------------------------------------------------------------------------------

-- Questão 1
menorMaior :: Ord a => [a] -> a -> ([a], [a])
menorMaior (x : xs) v = (separaMenor (x : xs) v, separaMaior (x : xs) v)

-----------------------------------------------------------------------------------------

-- Questão 2
colocaAsterisco :: [Char] -> Char -> [Char]
colocaAsterisco [] a = []
colocaAsterisco (x : xs) a
  | a /= x = x : colocaAsterisco xs a
  | otherwise = '*' : colocaAsterisco xs a

-----------------------------------------------------------------------------------------

-- Questão 3
insereElemento :: Ord a => a -> [a] -> [a]
insereElemento a [] = [a]
insereElemento a (x:xs)
  | a < x = a : x : xs
  | otherwise = x : insereElemento a xs

-----------------------------------------------------------------------------------------

-- Questão 4
mergeList :: Ord a => [a] -> [a] -> [a]
mergeList [] [] = []
mergeList (x : xs) [] = x : mergeList xs []
mergeList [] (y : ys) = y : mergeList [] ys
mergeList (x : xs) (y : ys) = sort $ x : y : mergeList xs ys

-----------------------------------------------------------------------------------------

-- Questão 5
contaPalavras :: String -> Int
contaPalavras xs = length $ words xs

-----------------------------------------------------------------------------------------

-- Questão 6
maiorOcorrencia :: Ord a => [a] -> (a, Int)
maiorOcorrencia xs = confereMaior $ map (\x -> (head x, length x)) $ group $ sort xs

-----------------------------------------------------------------------------------------