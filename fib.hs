fib n = helper 0 1 n
  where helper curr prev n
          | n == 0   = curr
          | n > 0    = helper (curr+prev) curr (n-1)
          | n < 0    = helper prev (curr-prev) (n+1)