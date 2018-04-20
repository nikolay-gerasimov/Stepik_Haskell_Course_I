meanList :: [Double] -> Double
meanList =  helper . foldr (\x (s1,s2) -> ((x+s1),s2+1)) (0,0) where
    helper (p1,p2) = (/) p1 p2