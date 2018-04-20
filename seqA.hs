seqA :: Integer -> Integer
seqA n = helper acc prev p_prev n
  where helper acc prev p_prev n
		  | n == 0 = acc
		  | n > 0 = helper (acc+prev-(2*p_prev)) 