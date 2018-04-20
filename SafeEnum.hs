instance SafeEnum Bool
instance SafeEnum Int

class (Enum a, Bounded a, Ord a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a 
    | a >= maxBound = minBound
	| otherwise = succ a
	
  spred :: a -> a
  spred a
    | a <= minBound = maxBound
	| otherwise = pred a
