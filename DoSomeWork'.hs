data Result' = Fail' Int | Success'

instance Show Result' where
    show (Fail' a)  = "Fail: " ++ show a
    show (Success') = "Success"
doSomeWork' :: SomeData -> Result'
doSomeWork' somedata = case doSomeWork somedata of
    (Fail, err) -> Fail' err
    (Success,_) -> Success'