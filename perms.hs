perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat(zipWith (++) (x) (perms xs))