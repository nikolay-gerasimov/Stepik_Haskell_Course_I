sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x:xs) (y:ys) (z:zs) = helper (x:xs) (y:ys) (z:zs) [] where
    helper (x:xs) (y:ys) (z:zs) result
		| ((length xs == 0) && (length ys == 0) && (length zs == 0)) = result
	    | otherwise = helper xs ys zs ((x+y+z):result)