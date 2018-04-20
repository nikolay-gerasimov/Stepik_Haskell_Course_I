evenOnly :: [a] -> [a]
evenOnly = fst . foldl (\(ys,pos) x -> if even pos then (ys++[x],pos+1) else (ys,pos+1)) ([],1) 