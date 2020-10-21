								-- SEMANA 27/07 --
-- concatenaFold 
concatenaFold [] = []
concatenaFold xs = foldr (++) [] xs

-- inverteFold
inverteFold xs = foldr (\x acc-> acc ++ [x]) [] xs

-- paridadeFold
paridadeFold xs = foldr (\x z -> if x then not z else z) True xs

-- duplicarFold
duplicarFold xs = foldr (\x acc -> if (elem x vogais) then (x:x:acc) else x:acc) [] xs
        where 
     	    vogais = "aeiouAEIOU"

-- filtraAplicaFold
filtraAplicaFold p f xs = foldr (\x acc -> if f x then (p x):acc else acc) [] xs

--mapFold
mapFold f xs = foldr (\x z -> f x : z) [] xs

-- removeLista
removeLista xs ys = foldr (\y z -> if elem y xs then z else y:z) [] ys

-- acertosFold
acertosFold xs ys = foldr f v [0..tam-1]
    where 
    tam   = length xs
    v     = 0
    f x z = if xs !! x == ys !! x then 1 + z else  z 

-- descompactaFold
descompactaFold xs = foldr (\(a,b) (xs,ys) -> (a:xs,b:ys) ) ([],[]) xs



								-- SEMANA 05/08 --
--KOLAKOSKI
kolakoski = 1 : 2 : 2 : concat [ replicate z y | (y,z) <- zip l1 l2  ]
    where
        l1    = cycle [1,2]  
        l2    = tail ( tail kolakoski)

-- hamming
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

-- collatz
collatz n = iterate (seguinte) n

seguinte 1 = 1
seguinte n = if mod n 2 == 0 then (n`div`2) else ((n*3)+1)

-- fechoKleene
fechoKleene xs = [""]++[ y++[x] | y<- (fechoKleene xs), x<-xs]

-- goldbach
    goldbach 4 = [(4,2,2)]
    goldbach n =goldbach (n-2) ++ (head [[(n,y,z)] | y<-(takeWhile (<n) primos),z<-(takeWhile (<n) primos),y+z == n])
     
    crivo :: [Int] -> [Int]
    crivo (p:xs) = p : crivo [x | x<-xs, x`mod`p/=0]
    
    primos :: [Int]
    primos = crivo [2..]

-- primosPalindromos
primosPalindromo = [i | i<-[2..], palindromo i && primo i]
     
palindromo n = show n == reverse (show n)
     
primo n = if (length (divisores n ))> 2 then False else True
     
divisores n = filter (\x -> mod n x == 0) [1..n]

-- primosGemeos
primos = [ i | i<-[2..], length (divisores i)== 2]

divisores n = filter (\x -> mod n x == 0) [1..n]

primosGemeos = [(a,b) | i <- [0..], ((primos!!(i+1))-(primos!!i)) == 2, (a,b)<-[((primos!!i),(primos!!(i+1)))]]


								-- SEMANA 12/08 --
-- arvoreExpressao

--LinkedList

-- mobile

-- makeMobile

--eqsplits
splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = [(x:a,b) | (a,b) <- zs] ++ [(a,x:b)| (a,b) <- zs]
    where zs = splits xs

eqsplits :: Num a => [a] -> [([a],[a])]
eqsplits xs = [(a,b)| (a,b)<-(splits xs), (sum a) == (sum b)] 

--splits
splits [] = [([],[])]
splits (x:xs) = [(x:a,b) | (a,b) <- zs] ++ [(a,x:b)| (a,b) <- zs]
	where zs = splits xs

-- numPassageiros
data Trem a = Vagao a ( Trem a ) | Vazio deriving Show
type Quantidade = Int
type Peso = Int
data Carga = SemCarga | Passageiro Quantidade | Mercadoria Peso deriving Show

numPassageiros :: Trem Carga -> Int
numPassageiros (Vazio) = 0
numPassageiros (Vagao (SemCarga) xs) = (numPassageiros(xs))
numPassageiros (Vagao (Mercadoria x) xs) = (numPassageiros(xs))
numPassageiros (Vagao (Passageiro x) xs) = x+(numPassageiros(xs))


								-- SEMANA 19/08 --
								
-- [Tipo Multiconjunto][Extra]remove
data MConj a = Vazia | No a Int (MConj a) (MConj a) deriving Show 

remove :: Ord a => a -> Int -> MConj a -> MConj a
remove x 0 m = m
remove x q Vazia = Vazia
remove x q (No a b Vazia xs) = if (a == x) then (if (b-q == 0) then xs else (No a (b-q) Vazia Vazia)) else (No a b Vazia (remove x q xs))
remove x q (No a b ys Vazia) = if (a == x) then (if (b-q == 0) then Vazia else (No a (b-q) Vazia Vazia)) else (No a b (remove x q ys) Vazia)
remove x q (No a b ys xs) = if (a == x) then (if (b-q == 0) then xs else (No a (b-q) ys xs)) else (No a b (remove x q ys)(remove x q xs))

-- [Tipo Arvore Binaria de Busca]insertArvore
data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)
 
insertArvore :: Ord a => a -> Arv a -> Arv a
insertArvore x (Vazia) = (No x (Vazia) (Vazia))
insertArvore x (No a (Vazia) (xs)) = if x <= a then (No a (No x Vazia Vazia) (xs)) else (No a (Vazia) (insertArvore x xs))
insertArvore x (No a (ys) (Vazia)) = if x <= a then (No a (insertArvore x ys) (Vazia)) else (No a (ys) (No x Vazia Vazia))
insertArvore x (No a (Vazia) (Vazia)) = if x <= a then (No a (No x Vazia Vazia) (Vazia)) else (No a (Vazia) (No x Vazia Vazia))
insertArvore x (No a (No b xs ys) (No c ns ms)) = if x <= a then (No a (insertArvore x (No b xs ys)) (No c ns ms)) else (No a (No b xs

-- [Tipo Arvore Binaria]foldTree
data Arvore a = Folha a | Ramo (Arvore a) (Arvore a) deriving (Show)

foldTree :: (a->b) -> (b->b->b) -> Arvore a -> b
foldTree f1 f2 (Folha a) = f1 a
foldTree f1 f2 (Ramo (Folha a) (Folha b)) = (f2 (f1 a) (f1 b))
foldTree f1 f2 (Ramo (xs) (Folha b)) =  (f2(foldTree f1 f2 xs) (f1 b))
foldTree f1 f2 (Ramo (Folha b) (xs) ) =  (f2(f1 b) (foldTree f1 f2 xs)) 
foldTree f1 f2 (Ramo (xs) (ys)) =  (f2 (foldTree f1 f2 xs) (foldTree f1 f2 ys))

-- [Tipo Multiset]makeMultiSet
data MultiSet a = MultiSet [(a,Int)] deriving (Show)
makeMultiSet :: Ord a => [a] -> MultiSet a
makeMultiSet xs = MultiSet (aux (bolha xs))

aux [] = []
aux (xs) = (head xs,length(takeWhile (\p -> p==head xs) xs)):aux (dropWhile (\p -> p==head xs) xs)

                 --FUNÇÃO DE ORDENAÇÃO BUBBLESORT
bolha :: (Ord a) => [a] -> [a]
bolha []     = []
bolha lista  = bolhaOrd lista (length lista)

bolhaOrd :: (Ord a) => [a] -> Int -> [a]
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

troca :: (Ord a) => [a] -> [a]
troca [x]     = [x]
troca (x:y:zs) | x > y      = y : troca (x:zs)
            | otherwise  = x : troca (y:zs)

-- [Tipo Arvore Binaria] menorNivelFolha
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

-- [Tipo Arvore Binaria]removeFolhas
data ArvBin a = Vazia | No a ( ArvBin a ) ( ArvBin a ) deriving (Show, Eq)

removeFolhas :: Eq a => ArvBin a -> ArvBin a
removeFolhas ( Vazia ) = Vazia
removeFolhas ( No x (esq) (dir)) | ((esq) == ( Vazia )) && ((dir) == ( Vazia )) = Vazia
                                 | ((esq) == ( Vazia )) = ( No x (Vazia) (removeFolhas dir))
                                 | ((dir) == ( Vazia )) = ( No x (removeFolhas esq) (Vazia))
                                 | otherwise = ( No x (removeFolhas esq) (removeFolhas dir))

-- [Tipo Arvore Binaria]cheia
data ArvBin a = Vazia | No a ( ArvBin a ) ( ArvBin a ) deriving (Show, Eq)

cheia :: Eq a => ArvBin a -> Bool
cheia ( Vazia ) = False
cheia (No x esq dir) | esq /= Vazia && dir /= Vazia = and[(cheia esq), (cheia dir)]
                     | esq == Vazia && dir /= Vazia = False
                     | esq /= Vazia && dir == Vazia = False
                     | otherwise = True

-- [Tipo MultiSet]insere
data MultiSet a = MultiSet [(a,Int)] deriving (Show)

insere :: Ord a => a -> MultiSet a -> MultiSet a
insere x xs = makeMultiSet1(x:(listMultiSet xs))

listMultiSet :: Ord a => MultiSet a -> [a]
listMultiSet (MultiSet xs) = [ x | (a,b)<-xs, x <- (take b (repeat a))]

makeMultiSet1 :: Ord a => [a] -> MultiSet a
makeMultiSet1 xs = MultiSet (aux (bolha xs))

aux [] = []
aux (xs) = (head xs, length(takeWhile (\p -> p==head xs) xs)):aux (dropWhile (\p -> p==head xs) xs)

bolha :: (Ord a) => [a] -> [a]
bolha []     = []
bolha lista  = bolhaOrd lista (length lista)

bolhaOrd :: (Ord a) => [a] -> Int -> [a]
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

troca :: (Ord a) => [a] -> [a]
troca [x]     = [x]
troca (x:y:zs) | x > y      = y : troca (x:zs)
            | otherwise  = x : troca (y:zs)   

-- [Tipo Multiset]delete
data MultiSet a = MultiSet [(a,Int)] deriving (Show)
 
deleteL a n [] = []
deleteL a n ((b,c):xs) |a == b && n >= c = xs
                       |a == b = ((b,c-n):xs)
                       |a < b = ((b,c):xs)
                       |otherwise  = (b,c) : (deleteL a n xs)
 
delete a n (MultiSet xs) = MultiSet (deleteL a n xs)


								-- SEMANA 14/09 --
-- [Classes de Tipos]NumerosComplexos
import Text.Printf
data Complex = Complex { real :: Float
                         ,img :: Float
                         } deriving (Eq)

c1 = Complex 2 3
c2 = Complex 5 4

somaComplexo (Complex a b) (Complex c d) = Complex (a+c) (b+d)                        
difComplexo (Complex a b) (Complex c d) = Complex (a-c) (b-d)
mulComplexo (Complex a b) (Complex c d) = Complex (a*c - b*d) (a*d + b*c)
apartirInteiro n = Complex (fromInteger n) 0
negativo (Complex a b) = Complex (-a) (-b)
absoluto (Complex a b) = Complex (sqrt (a*a + b*b)) 0
sinal c@(Complex a b) = Complex (a/r) (b/r)
     where r = real (absoluto c) 

instance Num Complex where
    (+) =  somaComplexo
    (-) = difComplexo
    (*) = mulComplexo
    fromInteger = apartirInteiro
    negate = negativo
    abs = absoluto
    signum = sinal  
 
instance Show Complex where
     show (Complex a b) = concat [printf "%.3f" a, " + ", printf "%.3f" b, "i"]  

-- [Classes de Tipos]Date
data Mes = Janeiro 
            | Fevereiro 
            | Marco 
            | Abril 
            | Maio 
            | Junho 
            | Julho 
            | Agosto 
            | Setembro 
            | Outubro
            | Novembro
            | Dezembro
            deriving (Eq, Ord, Show, Enum) 
 
data Date = MkDate { dia :: Int 
            , mes :: Mes    
            , ano :: Int   
            }

instance Show Date where
    show (MkDate a b c) =  show a ++ " de " ++ show b ++ " de " ++ show c    	
instance Eq Date where
     (==) (MkDate a b c) (MkDate d e f) = if a == d && b == e && c == f then True else False

 instance Ord Date where
     (<=) (MkDate a b c) (MkDate d e f) | c == f && a <= d && b <= e = True
                                        | c == f && a > d && b == e = False
                                        | c < f = True
                                        | otherwise = False 

-- [Classe de Tipos]Matrix
type Row = [Float]
data Matrix = Matrix { ncols :: Int
                    ,  nrows :: Int 
                    ,  rows :: [Row]
                    } 

            -- matriz de zeros
zeroMatrix :: Int -> Int -> Matrix
zeroMatrix l c = Matrix c l (take l (repeat (take c (repeat 0))))
            -- matriz de uns
oneMatrix :: Int -> Int -> Matrix
oneMatrix l c = Matrix c l (take l (repeat (take c (repeat 1))))

            -- matriz identidade : recebe ordem
identMatrix :: Int -> Matrix
identMatrix n = Matrix n n [ [ if i == j then 1 else 0 | j <- [0..(n-1)]] | i <- [0..(n-1)]]

            -- soma duas matrizes
sumMatrix :: Matrix -> Matrix -> Matrix
sumMatrix (Matrix n1 m1 xss) (Matrix n2 m2 yss) | n1 == n2 && m1 == m2 = Matrix n1 m1 [ [ (xss !! i) !! j + (yss !! i) !! j | j <- [0..(m1-1)]] | i <- [0..(n1-1)]]
                                                | otherwise = error "sumMatrix : dimensões diferentes" 
            -- produto de escalar por matriz
prodScalar :: Float -> Matrix -> Matrix
prodScalar k (Matrix n m xss) = Matrix n m [ [k*(xss !! i !! j)| j <- [0..(m-1)]] | i <- [0..(n-1)]]

prodInterno :: [Float] -> [Float] -> Float
prodInterno xs ys = sum $ zipWith (*) xs ys

getRow i (Matrix _ _ xss) = (xss !! i)
getCol j (Matrix l c xss) = [ (xss !! i) !! j | i<-[0..(l-1)] ]
            -- produto entre matrizes
prodMatrix :: Matrix -> Matrix -> Matrix
prodMatrix matrix1@(Matrix n1 m1 xss) matrix2@(Matrix n2 m2 yss) = Matrix n1 m2 [ [ pos i j | j <- [0..(m2-1)]]| i <- [0..(n1-1)]]
    where pos i j = prodInterno (getRow i matrix1) (getCol j matrix2)
 
            -- transforma listas de listas de
            -- floats numa matriz
listToMatrix :: [Row] -> Matrix
listToMatrix xss = Matrix n m xss
    where n = length xss
          m = length (xss !! 0)

showRow xs = "| " ++ concat [ show x ++ " "| x <- xs ] ++ "|\n"
 
showMatrix (Matrix n m xss) = concat [showRow xs | xs <- xss]

instance Show Matrix where
    show = showMatrix  

-- [Classe de Tipos]PedraPapelTesoura
import Data.List

data Gesto = Pedra | Papel | Tesoura deriving (Eq)
 
ganhadores :: [(Gesto,Gesto)] -> [Int]
ganhadores xs = remove_dups [ x | (a,b) <- xs, ganhaDe a b, x <-  (elemIndices (a,b) xs)]
 
ganhaDe :: Gesto -> Gesto -> Bool 
ganhaDe a b | a == Pedra && b == Tesoura = True
            | a == Papel && b == Pedra = True
            | a == Tesoura && b == Papel = True
            | otherwise = False
    
            --FUNÇÃO PARA REMOVER DUPLICADOS            
remove_dups :: (Ord a, Eq a) => [a] -> [a]
remove_dups xs = remove $ sort xs
   where
     remove []  = []
     remove [x] = [x]
     remove (x1:x2:xs)
       | x1 == x2  = remove (x1:xs)
       | otherwise = x1 : remove (x2:xs)

-- Pessoa
import Data.List
import Text.Printf
data Pessoa = Pessoa { nome :: String
            , idade :: Int
            , salario :: Float 
            } 
            
data Criterio = ByNome | ByIdade | BySalario deriving (Eq)

p1 = Pessoa "Joao" 25 2000.0
p2 = Pessoa "Ana" 20 2500.0
p3 = Pessoa "Alyson" 22 2200.0

pessoas = [ Pessoa "Joao" 25 2000, Pessoa "Ana" 20 2500, Pessoa "Alyson" 22 2200]
 
instance Show Pessoa where
show (Pessoa a b c) = concat [printf "%s " a, printf "tem %d anos " b, printf "e ganha de salario %f" c] 

sortListPessoa :: [Pessoa] -> Criterio -> [Pessoa]
sortListPessoa [] (ByNome) = []
sortListPessoa [] (ByIdade) = []
sortListPessoa [] (BySalario) = []
sortListPessoa (p1:ps) (ByNome) = sortListPessoa menores (ByNome) ++ iguais ++ [p1] ++ sortListPessoa maiores (ByNome)
    where
    menores = [ p | p <- ps, (compare (nome p) (nome p1)) == LT]
    maiores = [ p | p <- ps, (compare (nome p) (nome p1)) == GT]
    iguais = [ p | p <- ps, (compare (nome p) (nome p1)) == EQ]
     
sortListPessoa (p2:ps) (ByIdade) = sortListPessoa menoresI (ByIdade) ++ iguaisI ++ [p2] ++ sortListPessoa maioresI (ByIdade)
    where
    menoresI = [ p | p <- ps, (compare (idade p) (idade p2)) == LT]
    maioresI = [ p | p <- ps, (compare (idade p) (idade p2)) == GT]
    iguaisI = [ p | p <- ps, (compare (idade p) (idade p2)) == EQ]
     
sortListPessoa (p3:ps) (BySalario) = sortListPessoa menoresS (BySalario) ++ iguaisS ++ [p3] ++ sortListPessoa maioresS (BySalario)
    where
    menoresS = [ p | p <- ps, (compare (salario p) (salario p3)) == LT]
    maioresS = [ p | p <- ps, (compare (salario p) (salario p3)) == GT]
    iguaisS = [ p | p <- ps, (compare (salario p) (salario p3)) == EQ]

-- [Classe de Tipos]Polinomio
import Polinomio

makePol :: (Eq a, Num a) => [a] -> Polinomio a
makePol [] = polZero
makePol (x:xs) | x /= 0 = consPol x n pol
               | otherwise = makePol xs
    where n = (length xs)
          pol = makePol xs   

     --derivada :: (Eq a, Num a) => Polinomio a -> Polinomio a
 derivada :: (Eq a, Num a) => Polinomio a -> Polinomio a
 derivada p
   | n <= 0 = polZero 
   |otherwise = consPol (a*(fromIntegral n)) (n-1) (derivada q)
   where
   n = grau p
   a = coefLider p
   q = restoPol p

