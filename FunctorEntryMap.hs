-- Определите представителя класса Functor для типов данных Entry и Map. Тип Map представляет словарь, ключами которого являются пары:

-- В результате должно обеспечиваться следующее поведение: fmap применяет функцию к значениям в словаре, не изменяя при этом ключи.

-- GHCi> fmap (map toUpper) $ Map []
-- Map []

-- GHCi> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
-- Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap func (Entry (a,b) v) =  Entry (a,b) (func v)

instance Functor (Map k1 k2) where
    fmap _ (Map []) = Map []
    fmap func (Map xs) = Map (map (fmap func) xs)