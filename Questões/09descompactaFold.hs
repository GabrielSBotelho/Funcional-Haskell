descompactaFold xs = foldr (\(a,b) (xs,ys) -> (a:xs,b:ys) ) ([],[]) xs
