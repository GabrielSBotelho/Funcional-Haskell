paridadeFold xs = foldr (\x z -> if x then not z else z) True xs
