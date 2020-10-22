data MultiSet a = MultiSet [(a,Int)] deriving (Show)
makeMultiSet :: Ord a => [a] -> MultiSet a
makeMultiSet xs = MultiSet (aux (bolha xs))

aux [] = []
aux (xs) = (head xs,length(takeWhile (\p -> p==head xs) xs)):aux (dropWhile (\p -> p==head xs) xs)

                 --FUNÇÃO DE ORDENAÇÃO BUBBLESORT
bolha :: (Ord a) => [a] -> [a]
bolha []     = []
bolha lista  = bolhaOrd lista (length lista)

bolhaOrd :: (Ord a) => [a] -> Int -> [a]
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

troca :: (Ord a) => [a] -> [a]
troca [x]     = [x]
troca (x:y:zs) | x > y      = y : troca (x:zs)
            | otherwise  = x : troca (y:zs)
