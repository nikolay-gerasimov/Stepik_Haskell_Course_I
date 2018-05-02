data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show
instance Functor Tree where
    fmap func tree = case tree of
        Leaf Nothing        -> Leaf Nothing
        Leaf (Just a)       -> Leaf (Just (func a))
        Branch l (Nothing) r -> Branch (fmap func l) (Nothing) (fmap func r)
        Branch l (Just v) r -> Branch (fmap func l) (Just (func v)) (fmap func r)