import Data.List
revRange :: (Char,Char) -> [Char]
revRange (a,b) = reverse $ unfoldr g a where
        g = (\x -> if x > b then Nothing else Just (x,succ x))