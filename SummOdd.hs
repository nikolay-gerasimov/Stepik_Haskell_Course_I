sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then s+x else s) 0