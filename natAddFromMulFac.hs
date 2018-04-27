data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

fromInt :: Integer -> Nat
fromInt 0 = Zero
fromInt x = Suc $ fromInt (x-1)

add :: Nat -> Nat -> Nat
add Zero y = y
add x Zero = x
add (Suc x) (Suc y) = Suc (add x (Suc y))

mul :: Nat -> Nat -> Nat
mul Zero y = Zero
mul x Zero = Zero
mul (Suc x) (Suc y) = (Suc y) `add` (x `mul` Suc y)

fac :: Nat -> Nat
fac Zero = Suc Zero
fac (Suc x) = (Suc x) `mul` (fac x) 