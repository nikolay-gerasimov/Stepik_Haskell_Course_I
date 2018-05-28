import Control.Monad.State

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (updateTree tree) 1 where
  updateTree (Leaf _) = do
    n <- get      
    put (n+1)
    return $ Leaf n
  updateTree (Fork tl _ tr) = do
    t'  <- updateTree tl
    n'  <- get
    put (n'+1)
    t'' <- updateTree tr
    return $ Fork t' n' t''
