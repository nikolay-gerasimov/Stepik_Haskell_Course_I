isPalindrome :: Eq a => [a] -> Bool
isPalindrome [_] = True
isPalindrome [] = True
isPalindrome (x:xs)
	| x == reverse x = True
	| otherwise = False