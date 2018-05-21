import Control.Monad.State
fibStep :: State (Integer, Integer) ()
fibStep = do
   n <- get
   put (snd n, (fst n) + (snd n))
   return ()