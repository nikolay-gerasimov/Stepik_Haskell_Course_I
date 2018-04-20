--change :: (Ord a, Num a) => a -> [[a]]
coins = [2,3,7,10]
change 0 = [[]] 
change money = [ x:y | x <- coins, money>=x, y <- change (money-x)]