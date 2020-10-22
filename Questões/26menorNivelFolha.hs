data Arvbin a = Vazia | No a (Arvbin a) (Arvbin a) deriving (Show)

menorFolha :: Arvbin a -> Int
menorFolha (No x Vazia Vazia) = 0
menorFolha (No x esq Vazia) = menorFolha esq + 1
menorFolha (No x Vazia dir) = menorFolha dir + 1
menorFolha (No x esq dir) = m + 1
 	where
     	m = min (menorFolha esq) (menorFolha dir)
 
nivelF :: Int -> Arvbin a -> [a]
nivelF _ (Vazia)        = []
nivelF 0 (No x Vazia Vazia) = [x]
nivelF n (No x esq dir) = nivelF (n-1) esq ++ nivelF (n-1) dir

menorNivelFolha :: Arvbin a -> [a]
menorNivelFolha t = nivelF m t
 	where
 		m = menorFolha t
