data Nat = Zero
         | Succ Nat

add Zero m     = m
add (Succ n) m = Succ (add n m)


data Expr = Val Int
          | Add Expr Expr
          deriving Show

eval          :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code  = [Op]
data Op    = PUSH Int | ADD

exec                      :: Code -> Stack -> Stack
exec [] s                  = s
exec (PUSH n : c) s        = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (m + n : s)

comp          :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

