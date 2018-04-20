groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs) | x == head xs = 
						let (r:rs) = groupElems xs
						in (x : r) : rs
				  | otherwise = [x] : groupElems xs