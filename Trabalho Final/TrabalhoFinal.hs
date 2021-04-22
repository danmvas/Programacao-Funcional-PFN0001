import Data.List (sortBy)
import Data.Function (on)

trd :: (a, b, c) -> c
trd (a,b,c) = c

geraPosicaoDosItens :: [([Char],Int)] -> Int -> [([Char],Int,Int)]
geraPosicaoDosItens xs 1 = geraPosicaoDosItens' xs 1

geraPosicaoDosItens' :: [([Char],Int)] -> Int -> [([Char],Int,Int)]
geraPosicaoDosItens' (x:xs) n = (fst x, snd x, n) : geraPosicaoDosItens' xs (n+1)
geraPosicaoDosItens' [] _ = []

ordenaListaPelaPrioridade :: [([Char],Int,Int)] -> [([Char],Int,Int)]
ordenaListaPelaPrioridade xs = sortBy (compare `on` (\(a,b,c)->b)) xs

atualizaPrioridade :: [([Char],Int,Int)] -> Int -> [([Char],Int,Int)]
atualizaPrioridade xs 1 = atualizaPrioridade' (head xs) xs 1

atualizaPrioridade' :: ([Char],Int,Int)->[([Char],Int,Int)] -> Int -> [([Char],Int,Int)]
atualizaPrioridade' antecessor (x:xs) n
    | snd antecessor == snd x = (fst antecessor, n, trd antecessor) : (fst x, n ,trd x) : atualizaPrioridade x xs (n+1)
    | otherwise = (fst x, n,trd x) : atualizaPrioridade x xs (n+1)
atualizaPrioridade' _ [] _ = []

ordenaListaPelaOrdem :: [([Char],Int,Int)] -> [([Char],Int,Int)]
ordenaListaPelaOrdem xs 1 = ordenaListaPelaOrdem' (head xs) xs 1

ordenaListaPelaOrdem' :: ([Char],Int,Int)->[([Char],Int,Int)] -> Int -> [([Char],Int,Int)]
ordenaListaPelaOrdem' antecessor (x:xs) n
    | snd antecessor == snd x = (fst antecessor, snd antecessor, n) : (fst x, snd x , n) : ordenaListaPelaOrdem x xs (n+1)
    | otherwise = (fst x, snd x, n) : ordenaListaPelaOrdem x xs (n+1)
ordenaListaPelaOrdem' _ [] _ = []

imprimeListaAtualizada :: [([Char],Int)] -> [([Char],Int)]
imprimeListaAtualizada xs = ordenaListaPelaOrdem (atualizaPrioridade (ordenaListaPelaPrioridade (geraPosicaoDosItens xs 1)) 1)