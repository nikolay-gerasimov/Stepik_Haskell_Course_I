instance Functor SomeType where
    fmap f x = (flip (>>=)) (\x->return (f x)) x