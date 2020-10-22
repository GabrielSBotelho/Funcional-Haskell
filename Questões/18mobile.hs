data Mobile = Pendente Int | Barra Mobile Mobile deriving (Eq, Show)

balanceado :: Mobile -> Bool
balanceado (Pendente x) = True
balanceado (Barra (Pendente x) xs) = if x == (peso xs) then True else False
balanceado (Barra xs ys) = if (balanceado xs)&&(balanceado ys) then True else False
balanceado (Barra xs (Pendente x)) = if (peso xs) == x then True else False

peso :: Mobile -> Int
peso (Pendente x) = x
peso (Barra (Pendente x) xs) = (x+(peso xs))
peso (Barra xs ys) = (peso xs)+(peso ys)
peso (Barra xs (Pendente x)) = (x+(peso xs))
