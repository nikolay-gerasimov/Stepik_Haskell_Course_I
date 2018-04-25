maybeToList :: Maybe a -> [a]
maybeToList a = case a of
    Just a  -> [a]
    Nothing -> []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe as = Just (head as)