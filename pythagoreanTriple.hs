pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
    a <- [1..x]
    b <- [2..x]
    c <- [3..x]
    True <- return ((a^2 + b^2) == c^2)
    True <- return(a<b)
    return (a,b,c)