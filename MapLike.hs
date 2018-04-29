{- Ниже приведено определение класса MapLike типов, похожих на тип Map. Определите представителя MapLike для типа ListMap, определенного ниже как список пар ключ-значение. Для каждого ключа должно храниться не больше одного значения. Функция insert заменяет старое значение новым, если ключ уже содержался в структуре. -}

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []

    lookup key (ListMap []) = Nothing
    lookup key (ListMap ((k,v):xs)) = if key == k then (Just v) else lookup key (ListMap xs)

{-     --insert key value as = let as = ListMap ((k,v):xs) in let result = filter (key==k) as in if result == [] then ListMap (as ++ [(key,value)]) else ListMap (as {getListMap = [(key,value)]})
    insert key value cv@xs = if (result == []) then ListMap (as ++ [(key,value)]) else ListMap (cv {getListMap = [(key,value)]}) where
        result = filter (key == k) as
        as = ((k,v):xs)
        xs = ListMap as   -}
    insert k v (ListMap m) = ListMap (m' ++ [(k, v)])
        where
        m' = filter (\(k', _) -> k /= k') m

    delete k (ListMap m) = ListMap (m')
        where
        m' = filter (\(k',_) -> k /= k') m