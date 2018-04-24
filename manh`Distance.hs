data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord a1 b1) (Coord a2 b2) = sqrt((a2-a1)^2+(b2-b1)^2) 

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord a1 b1) (Coord a2 b2) = abs(a2-a1) + abs(b2-b1)