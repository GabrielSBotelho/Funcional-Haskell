data ArvBin a = Vazia | No a ( ArvBin a ) ( ArvBin a ) deriving (Show, Eq)

removeFolhas :: Eq a => ArvBin a -> ArvBin a
removeFolhas ( Vazia ) = Vazia
removeFolhas ( No x (esq) (dir)) | ((esq) == ( Vazia )) && ((dir) == ( Vazia )) = Vazia
                                 | ((esq) == ( Vazia )) = ( No x (Vazia) (removeFolhas dir))
                                 | ((dir) == ( Vazia )) = ( No x (removeFolhas esq) (Vazia))
                                 | otherwise = ( No x (removeFolhas esq) (removeFolhas dir))
