import Debug.Trace
data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

unsign :: Z -> [Bit]
unsign (Z _ zs) = zs

invertOne :: Bit -> Bit
invertOne Zero = One
invertOne One = Zero

invert :: [Bit] -> [Bit]
invert xs = map (invertOne) xs

dopCoded :: [Bit] -> [Bit]
dopCoded xs = unsign (Z Plus (invert xs) `add` (Z Plus [One]))

addOne :: Bit -> Bit -> Bit
addOne One One = Zero
addOne Zero Zero = Zero
addOne _ _ = One

add :: Z -> Z -> Z
add x y = let (x1:x1s) = fst (prepare (unsign x) (unsign y)) 
              (y1:y1s) = snd (prepare (unsign x) (unsign y))  in Z Plus (helper (x1:x1s) (y1:y1s) Zero) where
    helper (One:as) (One:bs)  One = (One : helper as bs One)  
    helper (One:as) (Zero:bs) One = (Zero : helper as bs One)
    helper (Zero:as) (One:bs) One = (Zero : helper as bs One)
    helper (One:as) (One:bs) Zero = (Zero : helper as bs One)
    helper (a:as) (b:bs) peren    = (((a `addOne` b) `addOne` peren) : helper as bs Zero)
    helper [] [] One              = [One]
    helper [] [] Zero             = []

prepare :: [Bit] -> [Bit] -> ([Bit],[Bit])
prepare a b = helper a b [] [] where
    helper (x:xs) (y:ys) accX accY = helper xs ys (accX++[x]) (accY++[y])
    helper [] (y:ys) accX accY = helper [] ys (accX++[Zero]) (accY++[y])
    helper (x:xs) [] accX accY = helper xs [] (accX++[x]) (accY++[Zero])
    helper [] [] accX accY = (accX,accY)

mul :: Z -> Z -> Z
mul = undefined