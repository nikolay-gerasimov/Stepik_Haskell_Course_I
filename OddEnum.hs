data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
instance Enum Odd where
	-- succ
	-- succ :: Odd -> Odd 
	succ (Odd a) = Odd (a+2)
	-- pred
	-- pred :: Odd -> Odd 
	pred (Odd a) = Odd (a-2)
	-- toEnum
	-- toEnum :: Int -> Odd
	toEnum a = (Odd (toInteger a))
	-- fromEnum
	-- fromEnum :: Odd -> Int
	fromEnum (Odd a) = fromIntegral a
	-- enumFrom
	-- enumFrom :: Odd -> [Odd]
	enumFrom (Odd a) = (Odd a):enumFrom(Odd(a+2)) 
	-- enumFromThen
	-- enumFromThen :: Odd -> Odd -> [Odd]
	enumFromThen (Odd a) (Odd b) = (Odd a):enumFromThen (Odd b) (Odd (b+b-a))
	-- enumFromTo
	-- enumFromTo :: Odd -> Odd -> [Odd]
	enumFromTo (Odd a) (Odd b) = take (fromIntegral (div (b-a) 2) + 1) $ enumFrom (Odd a) 
	-- enumFromThenTo
	-- enumFromThenTo :: Odd -> Odd -> Odd -> [Odd]
	enumFromThenTo (Odd a) (Odd b) (Odd c) = take (fromIntegral(div (c-b) (b-a)) +2) $ enumFromThen (Odd a) (Odd b)