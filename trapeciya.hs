integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * (big_sum + (helper (a+h) 0)) where
   h = (b - a) / n
   n = 1000
   big_sum = (f a + f b) / 2
   helper xi sum
     | abs(b-xi)<=0.0001 = sum
     | otherwise = helper (xi+h) (abs(f xi) + sum)