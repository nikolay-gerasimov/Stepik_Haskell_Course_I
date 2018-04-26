data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a (xs)) = [a]++(fromList (xs))

toList :: [a] -> List a
toList [] = Nil
toList (a:xs) = Cons a (toList xs)