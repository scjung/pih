data Nat = Zero | Succ Nat

instance Show Nat where
  show n = show (nat2int n)

nat2int         :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat        :: Int -> Nat
int2nat 0       = Zero
int2nat (n + 1) = Succ (int2nat n)

add           :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult              :: Nat -> Nat -> Nat
mult _ Zero        = Zero
mult n (Succ Zero) = n
mult n (Succ m)    = add n (mult n m)