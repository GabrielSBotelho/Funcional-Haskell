inverteFold xs = foldr (\x acc-> acc ++ [x]) [] xs
