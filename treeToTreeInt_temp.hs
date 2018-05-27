import Control.Monad.State

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

numberTree :: Tree () -> Tree Integer
numberTree tree = do
      n <- get
      case tree of
           Leaf _ -> do 
                put (n+1)
                return Leaf n
           Fork tl _ tr -> do
                t'  <- numberTree tl
                n'  <- get
                t'' <- numberTree tr
                put n
                return $ Fork t' n' t''
      result <- get
      return $ fst result
