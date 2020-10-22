data Arvore a = Folha a | Ramo (Arvore a) (Arvore a) deriving (Show)

foldTree :: (a->b) -> (b->b->b) -> Arvore a -> b
foldTree f1 f2 (Folha a) = f1 a
foldTree f1 f2 (Ramo (Folha a) (Folha b)) = (f2 (f1 a) (f1 b))
foldTree f1 f2 (Ramo (xs) (Folha b)) =  (f2(foldTree f1 f2 xs) (f1 b))
foldTree f1 f2 (Ramo (Folha b) (xs) ) =  (f2(f1 b) (foldTree f1 f2 xs)) 
foldTree f1 f2 (Ramo (xs) (ys)) =  (f2 (foldTree f1 f2 xs) (foldTree f1 f2 ys))
