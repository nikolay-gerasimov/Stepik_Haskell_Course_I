import Data.Char
readDigits :: String -> (String, String)
readDigits a = span isDigit a