data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + (height a) `max` (height b) 

size :: Tree a -> Int
size (Leaf a) = 1
size (Node (Leaf a) (Leaf b)) = 3
size (Node a b) = 1 + size a + size b

avg :: Tree Integer -> Integer
avg t =
    let (c,s) = go t
    in s `div` c
    
go :: Tree Integer -> (Integer,Integer)
go (Leaf a) = (1,a)
go (Node a b) = go a `plus` go b

plus :: (Num a) => (a,a) -> (a,a) -> (a,a)
plus (a,b) (x,y) = (a+x,b+y)

data Treeleaves = L | R deriving Show

tree  = Node (Node (Node (Leaf 100) (Leaf 200)) (Leaf 201))(Leaf 303)
tree1 = Node (Node (Node (Leaf L) (Leaf R)) (Leaf R))(Leaf R)