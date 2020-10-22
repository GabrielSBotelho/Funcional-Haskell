hamming = 1 : mescla3 l1 l2 l3
    where
       l1 = [ 2*i | i <- hamming]
       l2 = [ 3*i | i <- hamming]
       l3 = [ 5*i | i <- hamming]
       
    mescla3 xs ys zs = merge xs (merge ys zs)
    
    merge (x:xs) (y:ys) 
      | x == y = merge xs (y:ys)
      | x < y  = x : merge xs    (y:ys)
      | y < x  = y : merge (x:xs) ys
