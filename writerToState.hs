writerToState :: Monoid w => Writer w a -> State w a
--writerToState :: (a,w) -> w->(a,w')
writerToState m = State $ \x -> (fst (runWriter m), x `mappend` (snd (runWriter m))) 