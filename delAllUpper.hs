import Data.Char
delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words