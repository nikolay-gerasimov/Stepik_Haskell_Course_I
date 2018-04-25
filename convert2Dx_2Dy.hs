data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x1 y1) = (Coord ( (fromIntegral x1) * size + 0.5 * size) ((fromIntegral y1) * size + 0.5 * size))

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x1 y1) = (Coord r1 r2) where
    r1 = floor (x1 / size)
    r2 = floor (y1 / size)