sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = helper (abs x) 0 0
    where
        helper 0 sum count = (sum, count)
        helper n sum count = let num = (n `div` 10)
            in helper num (sum + n - num * 10) (count + 1)