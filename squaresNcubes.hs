squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concat . map (\x -> [x^2,x^3])