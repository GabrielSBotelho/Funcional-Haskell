data MultiSet a = MultiSet [(a,Int)] deriving (Show)
 
deleteL a n [] = []
deleteL a n ((b,c):xs) |a == b && n >= c = xs
                       |a == b = ((b,c-n):xs)
                       |a < b = ((b,c):xs)
                       |otherwise  = (b,c) : (deleteL a n xs)
 
delete a n (MultiSet xs) = MultiSet (deleteL a n xs)
