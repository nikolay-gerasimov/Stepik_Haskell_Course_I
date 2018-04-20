range :: Integral a => a -> a ->[a]
range a b
   | a < b     = (a : (range (a+1) b))
   | a > b     = (a : (range (a-1) b))
   | otherwise = [b]