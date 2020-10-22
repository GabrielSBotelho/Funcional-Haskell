data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)
 
insertArvore :: Ord a => a -> Arv a -> Arv a
insertArvore x (Vazia) = (No x (Vazia) (Vazia))
insertArvore x (No a (Vazia) (xs)) = if x <= a then (No a (No x Vazia Vazia) (xs)) else (No a (Vazia) (insertArvore x xs))
insertArvore x (No a (ys) (Vazia)) = if x <= a then (No a (insertArvore x ys) (Vazia)) else (No a (ys) (No x Vazia Vazia))
insertArvore x (No a (Vazia) (Vazia)) = if x <= a then (No a (No x Vazia Vazia) (Vazia)) else (No a (Vazia) (No x Vazia Vazia))
insertArvore x (No a (No b xs ys) (No c ns ms)) = if x <= a then (No a (insertArvore x (No b xs ys)) (No c ns ms)) else (No a (No b xs
