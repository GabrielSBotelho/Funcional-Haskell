splits [] = [([],[])]
splits (x:xs) = [(x:a,b) | (a,b) <- zs] ++ [(a,x:b)| (a,b) <- zs]
	where zs = splits xs
