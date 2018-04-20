data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area shape = case shape of
    (Circle r)      -> pi*r*r
    (Rectangle a b) -> a*b