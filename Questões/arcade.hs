----------- alt a
soma :: Num a => a -> a -> a
soma x y = x + y

----------- alt b 
miolo xs = drop 1 (reverse (drop 1 (reverse xs)))

----------- alt c 
equal3 a b c | a == b && a == c = 3
             | a == b && a /= c = 2
             | a /= b && a == c = 2
             | a /= b && b == c = 2
             | otherwise = 0
----------- alt d 
maiorDeTres x y z | x >= y && x >= z = x
                  | y >= z = y
                  | otherwise = z   
----------- alt e 
somaImpares xs = sum [x | x <- xs , odd x]
----------- alt f 
idImpares xs = sum [ 1 | x <- xs , x < 0]
----------- alt g 
final g xs = reverse (take g (reverse xs))
----------- alt h
gangorra a b c d 
    | a*b == c*d = 0
    | a*b > c*d = -1
    | otherwise = 1
