primos = [ i | i<-[2..], length (divisores i)== 2]

divisores n = filter (\x -> mod n x == 0) [1..n]

primosGemeos = [(a,b) | i <- [0..], ((primos!!(i+1))-(primos!!i)) == 2, (a,b)<-[((primos!!i),(primos!!(i+1)))]]
