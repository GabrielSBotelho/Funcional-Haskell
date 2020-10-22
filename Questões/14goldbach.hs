    goldbach 4 = [(4,2,2)]
    goldbach n =goldbach (n-2) ++ (head [[(n,y,z)] | y<-(takeWhile (<n) primos),z<-(takeWhile (<n) primos),y+z == n])
     
    crivo :: [Int] -> [Int]
    crivo (p:xs) = p : crivo [x | x<-xs, x`mod`p/=0]
    
    primos :: [Int]
    primos = crivo [2..]
