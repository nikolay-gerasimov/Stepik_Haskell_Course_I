readerToState :: Reader r a -> State r a
--readerToState :: (r->a) -> r->(a,r)
readerToState m = State $ \e -> (runReader m e,e) 