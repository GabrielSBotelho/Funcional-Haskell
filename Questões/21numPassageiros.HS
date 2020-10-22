data Trem a = Vagao a ( Trem a ) | Vazio deriving Show
type Quantidade = Int
type Peso = Int
data Carga = SemCarga | Passageiro Quantidade | Mercadoria Peso deriving Show

numPassageiros :: Trem Carga -> Int
numPassageiros (Vazio) = 0
numPassageiros (Vagao (SemCarga) xs) = (numPassageiros(xs))
numPassageiros (Vagao (Mercadoria x) xs) = (numPassageiros(xs))
numPassageiros (Vagao (Passageiro x) xs) = x+(numPassageiros(xs))
