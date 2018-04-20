class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab where
	  | doesEnrageMork a == True =  stomp a
	  | doesEnrageGork a == True = stab a
	  | doesEnrageGork a == True && doesEnrageMork a == True = stomp (stab a)
	  | _ = a