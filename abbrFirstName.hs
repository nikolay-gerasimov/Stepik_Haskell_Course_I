data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName (Person{firstName=fn,lastName=ln,age=ag}) = if (length fn >= 2) then (Person{firstName = [(head fn)] ++ ".",lastName=ln,age=ag}) else (Person{firstName = fn,lastName=ln,age=ag})
