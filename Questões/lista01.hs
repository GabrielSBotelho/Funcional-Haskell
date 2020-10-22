--Q1)
menorDD x y | x > y = y
            | x < y = x 
            | otherwise = error "iguais"

menorDeDois x y | x > y && x /= y+1 = y + 1
                | x < y && y /= x+1 = x + 1
                | otherwise = error "ERROR"

-- Q2)
menorDeTres x y z | x <= y && x <= z = x
                  | y <= z = y
                  | otherwise = z

maiorDeTres x y z | x >= y && x >= z = x
                  | y >= z = y
                  | otherwise = z      

-- Questao 03
--fac :: (Integral a) => a -> a
fact 0 = 1
fact n = n*fact(n-1)

--Questão 05
elemento n [] = []
elemento 0 (x:xs) = [x]
elemento n xs = [xs!!n]  

--Questão 06
pertence1 n [] = False
pertence1 n [x] = if n /= x then False else True
pertence1 n (x:xs) | x == n = True
                   |otherwise = pertence1 n xs  

--Questão 07
total [] = 0
total [x] = 1
total (x:xs) = 1 + total xs

--Questão 08
max1 x y | x > y = x
         | x < y = y
         | otherwise = error "Erro"

maximo1 xs = case xs of [] -> error "lista vazia"
                        [x] -> x 
                        (x:t) -> max1 x (maximo1 t)

--Questão 09
frequencia n [] = 0
frequencia n [x] = if x == n then 1 else 0
frequencia n (x:xs) | n == x = 1 + frequencia n xs
                    | otherwise = frequencia n xs  

-- Questão 10
unico n [] = False
unico n [x] = if n == x then True else False
unico n (x:xs) | n /= x = unico n xs
               | n == x = not (unico n xs)

-- Questão 11
maioresQue n [] = []
maioresQue n [x] = if x > n then [x] else []
maioresQue n xs = [ ys | ys <- xs, ys > n]

--Questão 12
concat1 [] ys = ys
concat1 [] [] = []
concat1 xs [] = xs
concat1 xs ys = xs ++ ys 

--Questão 13
calda [] = []
calda [x] = []
calda (x:xs) = xs

--Questão 14
corpo [] = []
corpo [x] = []
corpo (x:xs) = (x:corpo xs)

--Questão 15
unique [] = []
unique [x] = [x]
unique (x:xs) = if elem x xs 
                   then unique xs 
                   else (x:unique xs)

--Questão 17
alter 0 = []
alter a = alter (c)++ [(-a), a]
          where c = a - 1

--Questão 18
reverso [] = []
reverso [a] = [a]
reverso xs = ((last xs):reverso (init xs))

--Questão 19
divide  xs 0 = ([] , xs)
divide xs n = (take n xs , drop n xs) 

--Questão 20
--intercal [] b = b
--intercal a [] = a
--intercal (y:a) (x:b) = [y,x]++intercal

--Questão 21
remove x y | x == y = []
           | otherwise = [y]

rem1 x [] = []
rem1 x [a] = if x == a then [] else [a]
rem1 x (y:ys) = remove x y ++ rem1 x ys --(remove x y : rem1 x ys)

uniao [] ys = ys
uniao xs [] = xs
uniao (x:xs) ys | elem x ys == True = uniao (x:xs) (rem1 x ys)
                |otherwise = (x:uniao xs ys) 

--Questão 22 rem1 feita na questão anterior
intersec [] ys = []
intersec xs [] = []
intersec (x:xs) ys | elem x ys == True = (x:intersec xs ys)
                   | otherwise = intersec xs (rem1 x ys)

--Q24)
inserir 0 xs = [0]++xs
inserir n [x] = if x < n then [x]++[n] else [n]++[x]
inserir n (x:xs) | x < n = (x: inserir n xs)
                 | otherwise = [n]++[x]++xs
--Q25)
isSorted [] = True
isSorted [x] = True 
isSorted(x:xs) |x < head xs = isSorted xs
               |otherwise = False

--Q26)
quick [] = []
quick (x:xs) = a ++ [x] ++ b
    where a = quick [n |n <- xs, n <= x]
          b = quick [n |n <- xs, n > x]

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x < y = x:( merge xs (y:ys) )
    | otherwise = y:( merge (x:xs) ys )

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort (x:[]) = [x]
mergesort xs = merge a b
    where n = div (length xs) 2
          a = mergesort $ take n xs
          b = mergesort $ drop n xs    

bubblesort'iter :: (Ord a) => [a] -> [a]
bubblesort'iter (x:y:xs)
    | x > y = y : bubblesort'iter (x:xs)
    | otherwise = x : bubblesort'iter (y:xs)
bubblesort'iter (x) = (x)

bubblesort' :: (Ord a) => [a] -> Int -> [a]
bubblesort' xs i 
    | i == (length xs) = xs
    | otherwise = bubblesort' (bubblesort'iter xs) (i + 1) 
 
bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblesort' xs 0     

--questão 27
rotateEsq [] _ = []
rotateEsq x 0 = x
rotateEsq x y
    | y > 0 = rotateEsq (tail x ++ [head x]) (y-1)
    | otherwise = error "Erro"

--questão 28
rotateDir [] _ = []
rotateDir x 0 = x
rotateDir x y
    | y > 0 = rotateDir ([last x] ++ init x) (y-1)
    | otherwise = error "Erro"    

-- Q31)
selecao [] _ = []
selecao xs d = [xs !! i | i <- d, i >= 0 && i < length xs ]


-- Rotate Left
rotate [] _ = []
rotate x 0 = x
rotate x y
  | y > 0 = rotate (tail x ++ [head x]) (y-1)
  | otherwise = rotate (last x : init x) (y+1)

-- Ratate Right 
rotated [] _ = []
rotated x 0 = x
rotated x y
    | y > 0 = rotated (last x : init x) (y-1)

-- Fibonacci 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
