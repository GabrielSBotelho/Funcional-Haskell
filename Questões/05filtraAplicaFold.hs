filtraAplicaFold p f xs = foldr (\x acc -> if f x then (p x):acc else acc) [] xs
