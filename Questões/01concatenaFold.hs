concatenaFold [] = []
concatenaFold xs = foldr (++) [] xs
