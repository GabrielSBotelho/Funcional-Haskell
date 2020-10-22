data ArvBin a = Vazia | No a ( ArvBin a ) ( ArvBin a ) deriving (Show, Eq)

cheia :: Eq a => ArvBin a -> Bool
cheia ( Vazia ) = False
cheia (No x esq dir) | esq /= Vazia && dir /= Vazia = and[(cheia esq), (cheia dir)]
                     | esq == Vazia && dir /= Vazia = False
                     | esq /= Vazia && dir == Vazia = False
                     | otherwise = True
