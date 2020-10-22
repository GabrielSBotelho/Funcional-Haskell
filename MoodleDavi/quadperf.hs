main = do
a <- readLn :: IO Int
print $ quadperf a

quadperf n = not(null m)
    where m = [x | x <- [0..n], x^2 == n]
