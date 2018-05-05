import Data.Char
strToDigit :: String -> Int
strToDigit xs = if all isDigit xs then helper xs (length xs) 0 else (-1) where
    helper [] 0 acc = acc
    helper (y:ys) l acc = helper ys (l-1) (acc+(digitToInt(y)*10^(l-1)))  