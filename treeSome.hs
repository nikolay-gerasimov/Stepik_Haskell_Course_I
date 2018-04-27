data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + (height a) `max` (height b) 

size :: Tree a -> Int
size (Leaf a) = 1
size (Node (Leaf a) (Leaf b)) = 3
size (Node a b) = 1 + size a + size b

data Treeleaves = L | R deriving Show

tree = Node (Node (Node (Leaf R) (Leaf L)) (Leaf R))(Leaf R)