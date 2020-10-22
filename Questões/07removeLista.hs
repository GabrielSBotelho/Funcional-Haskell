removeLista xs ys = foldr (\y z -> if elem y xs then z else y:z) [] ys
