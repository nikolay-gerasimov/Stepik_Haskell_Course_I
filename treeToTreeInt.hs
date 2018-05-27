import Control.Monad.State

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

numberTree :: Tree () -> Tree Integer
numberTree tree = do
      (t,n) <- get
      case tree of
           Leaf _ -> put (Leaf n,n+1)
           Fork tl _ tr -> do
                t'  <- numberTree tl
                n'  <- get
                t'' <- numberTree tr
                put (Fork t' (snd n') t'')
