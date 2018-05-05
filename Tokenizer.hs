import Data.Char
import Data.List
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace deriving (Eq, Show)

strToDigit :: String -> Int
strToDigit xs = helper xs (length xs) 0 where
    helper [] 0 acc = acc
    helper (y:ys) l acc = helper ys (l-1) (acc+(digitToInt(y)*10^(l-1)))

asToken :: String -> Maybe Token
asToken xs = case xs of
    "+" -> Just Plus
    "-" -> Just Minus
    "(" -> Just LeftBrace
    ")" -> Just RightBrace
    xs  -> if all isDigit xs then Just (Number (strToDigit xs)) else Nothing

tokenize :: String -> Maybe [Token]
tokenize "" = Just []
tokenize xs = mapM asToken (words xs)