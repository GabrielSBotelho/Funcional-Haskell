equal3 a b c | a == b && a == c = 3
             | a == b && a /= c = 2
             | a /= b && a == c = 2
             | a /= b && b == c = 2
             | otherwise = 0
