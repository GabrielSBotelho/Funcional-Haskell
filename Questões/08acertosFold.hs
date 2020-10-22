acertosFold xs ys = foldr f v [0..tam-1]
    where 
    tam   = length xs
    v     = 0
    f x z = if xs !! x == ys !! x then 1 + z else  z 
