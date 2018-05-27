import Control.Monad.State

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberTree :: Tree Char -> Tree Integer
numberTree tree = helper 0 where
    helper n = case tree of
        Leaf _ -> Leaf n
        Fork left mid right -> Fork (numberTree left) n (numberTree right)
