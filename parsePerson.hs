data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parseCheck :: String -> Bool
parseCheck xs = 

parsePerson :: String -> Either Error Person
parsePerson = undefined