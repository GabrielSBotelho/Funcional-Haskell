main = do
a <- readLn :: IO [Int]
print $ unique a

unique [] = []
unique [x] = [x]
unique (x:xs) = if elem x xs 
                then unique xs 
                else (x:unique xs)
