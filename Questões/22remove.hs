data MConj a = Vazia | No a Int (MConj a) (MConj a) deriving Show 

remove :: Ord a => a -> Int -> MConj a -> MConj a
remove x 0 m = m
remove x q Vazia = Vazia
remove x q (No a b Vazia xs) = if (a == x) then (if (b-q == 0) then xs else (No a (b-q) Vazia Vazia)) else (No a b Vazia (remove x q xs))
remove x q (No a b ys Vazia) = if (a == x) then (if (b-q == 0) then Vazia else (No a (b-q) Vazia Vazia)) else (No a b (remove x q ys) Vazia)
remove x q (No a b ys xs) = if (a == x) then (if (b-q == 0) then xs else (No a (b-q) ys xs)) else (No a b (remove x q ys)(remove x q xs))
