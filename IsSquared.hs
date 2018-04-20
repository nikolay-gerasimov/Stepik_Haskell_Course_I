data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare shape = case shape of
   (Rectangle a b) -> a==b
   (_)      -> False   