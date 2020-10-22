primosPalindromo = [i | i<-[2..], palindromo i && primo i]
     
palindromo n = show n == reverse (show n)
     
primo n = if (length (divisores n ))> 2 then False else True
     
divisores n = filter (\x -> mod n x == 0) [1..n]
