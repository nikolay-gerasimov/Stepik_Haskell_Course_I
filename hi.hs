--repeatuntil :: IO (String)
repeatuntil = do
   putStrLn "What is your name?"
   putStr "Name: "
   name <- getLine
   if null name then repeatuntil else return name

--main' :: IO ()
main' = do
    name <- repeatuntil 
    putStrLn("Hi, "++name++"!")