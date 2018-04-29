newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    mappend (Maybe' Nothing) _ = Maybe' Nothing 
    mappend _ (Maybe' Nothing) = Maybe' Nothing 
    mappend (Maybe' x) (Maybe' y) = Maybe' (x `mappend` y)