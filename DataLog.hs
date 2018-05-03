data Log a = Log [String] a deriving Show
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg param = Log [msg] (f param) 

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = case (f x) of
    Log msg res -> case (g res) of
        Log msg1 res1 -> Log (msg++msg1) res1

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg value) klyasli = case (klyasli value) of
    (Log msg1 value2) -> Log (msg++msg1) value2

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList param [] = return param
execLoggersList param xs = foldl (>>=) (Log [] param) xs   
--    case (x param) of
--    (Log msg p2) -> (x param) >>= ((1head xs) p2) 

returnLog :: a -> Log a
returnLog x = Log [] x

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"