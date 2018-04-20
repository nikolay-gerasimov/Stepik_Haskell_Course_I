max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 [] [] [] = []
max3 (x:xs) (y:ys) (z:zs) = maximum[x,y,z] : max3 xs ys zs