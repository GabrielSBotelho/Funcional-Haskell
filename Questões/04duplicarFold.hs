duplicarFold xs = foldr (\x acc -> if (elem x vogais) then (x:x:acc) else x:acc) [] xs
        where 
     	  vogais = "aeiouAEIOU"
