filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj pred1 pred2 (x:xs)
	| pred1 x || pred2 x = (x:(filterDisj pred1 pred2 xs))
	| otherwise = filterDisj pred1 pred2 xs