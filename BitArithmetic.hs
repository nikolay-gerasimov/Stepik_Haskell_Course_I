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

dopCoded :: Z -> Z
dopCoded (Z Minus xs) = (Z Plus (invert xs)) `add` (Z Plus [One])
dopCoded (Z Plus xs) = Z Plus xs

dopCodedPair :: (Z,Z) -> (Z,Z)
dopCodedPair (xs,ys) = (dopCoded xs,dopCoded ys) 

addOne :: Bit -> Bit -> Bit
addOne One One = Zero
addOne Zero Zero = Zero
addOne _ _ = One

add :: Z -> Z -> Z
--Known bugs: we should do prepare with dopcode convert
add x y = let (x1:x1s) = unsign (fst (prepare x y))
              (y1:y1s) = unsign (snd (prepare x y))  in Z Plus (helper (x1:x1s) (y1:y1s) Zero) where
    helper (One:as) (One:bs)  One = (One : helper as bs One)  
    helper (One:as) (Zero:bs) One = (Zero : helper as bs One)
    helper (Zero:as) (One:bs) One = (Zero : helper as bs One)
    helper (One:as) (One:bs) Zero = (Zero : helper as bs One)
    helper (a:as) (b:bs) peren    = (((a `addOne` b) `addOne` peren) : helper as bs Zero)
    helper [] [] One              = [One]
    helper [] [] Zero             = []

prepare :: Z -> Z -> (Z,Z)
prepare (Z signA a) (Z signB b) = helper a b [] [] where
    helper (x:xs) (y:ys) accX accY = helper xs ys (accX++[x]) (accY++[y])
    helper [] (y:ys) accX accY = helper [] ys (accX++[Zero]) (accY++[y])
    helper (x:xs) [] accX accY = helper xs [] (accX++[x]) (accY++[Zero])
    helper [] [] accX accY = (Z signA accX,Z signB accY)

mul :: Z -> Z -> Z
mul = undefined