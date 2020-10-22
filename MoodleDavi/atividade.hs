-- unique
main = do
a <- readLn :: IO [Int]
print $ unique a

unique [] = []
unique [x] = [x]
unique (x:xs) = if elem x xs 
                then unique xs 
                else (x:unique xs)

--  qsort
main = do
a <- readLn :: IO [Int]
print $ qsort a
 
qsort [] = []
qsort (x:xs) = a ++ [x] ++ b
    where a = qsort [n |n <- xs, n <= x]
          b = qsort [n |n <- xs, n > x]

-- merge
main = do
     a <- readLn :: IO [Int]
    b <- readLn :: IO [Int]
    print $ merge a b
     
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x < y = x:( merge xs (y:ys) )
    | otherwise = y:( merge (x:xs) ys )     

-- quadperf

main = do
a <- readLn :: IO Int
print $ quadperf a

quadperf n = not(null m)
    where m = [x | x <- [0..n], x^2 == n]