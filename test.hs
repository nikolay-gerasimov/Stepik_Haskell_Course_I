divideby10 x sum count
		| (x mod 10) == 0 = (sum , count)
		| (x mod 10) > 0 = divideby10 (div x 10) (sum + mod x 10) (count+1)