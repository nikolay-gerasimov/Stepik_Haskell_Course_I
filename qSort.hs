qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort(filter (<x) xs) ++ (filter (==x)(x:xs)) ++ qsort(filter (>x) xs)