main = do
a <- readLn :: IO [Int]
print $ qsort a
 
qsort [] = []
qsort (x:xs) = a ++ [x] ++ b
    where a = qsort [n |n <- xs, n <= x]
          b = qsort [n |n <- xs, n > x]
