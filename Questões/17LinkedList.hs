data LinkedList a = Vazia | No1 a (LinkedList a) deriving (Eq, Show)

fromList :: [a] -> LinkedList a
fromList [] = Vazia
fromList (x:xs) = No1 x (fromList xs)

toList :: LinkedList a -> [a]
toList Vazia = []
toList (No1 x (xs)) = (x:toList xs)

append :: a -> LinkedList a -> LinkedList a
append x xs = fromList((toList xs) ++ [x])

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList l = fromList (reverse(toList l))
