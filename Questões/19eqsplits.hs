splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = [(x:a,b) | (a,b) <- zs] ++ [(a,x:b)| (a,b) <- zs]
    where zs = splits xs

eqsplits :: Num a => [a] -> [([a],[a])]
eqsplits xs = [(a,b)| (a,b)<-(splits xs), (sum a) == (sum b)] 
