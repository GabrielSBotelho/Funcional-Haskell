kolakoski = 1 : 2 : 2 : concat [ replicate z y | (y,z) <- zip l1 l2  ]
    where
        l1    = cycle [1,2]  
        l2    = tail ( tail kolakoski)
