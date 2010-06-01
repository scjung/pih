---
--- 10.1 Type declarations
---
type Pos = (Int, Int)

type Assoc k v = [(k, v)]

find    :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

---
--- 10.2 Data declarations
---
data Move = Left' | Right' | Up' | Down'

move              :: Move -> Pos -> Pos
move Left' (x, y)  = (x - 1, y)
move Right' (x, y) = (x + 1, y)
move Up' (x, y)    = (x, y + 1)
move Down' (x, y)  = (x, y + 1)

moves           :: [Move] -> Pos -> Pos
moves [] p       = p
moves (m : ms) p = moves ms (move m p)

flip       :: Move -> Move
flip Left'  = Right'
flip Right' = Left'
flip Up'    = Down'
flip Down'  = Up'

data Shape = Circle Float | Rect Float Float

square  :: Float -> Shape
square n = Rect n n

area           :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

safediv    :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead   :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

---
--- 10.3 Recursive types
---
data Nat = Zero | Succ Nat

nat2int         :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat        :: Int -> Nat
int2nat 0       = Zero
int2nat (n + 1) = Succ (int2nat n)

add           :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

data List a = Nil | Cons a (List a)

len            :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

data Tree = Leaf Int | Node Tree Int Tree

t :: Tree
t  = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs               :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r)
  | m == n            = True
  | m < n             = occurs m l
  | m > n             = occurs m r

flatten             :: Tree -> [Int]
flatten (Leaf n)     = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

---
--- 10.4 Tautology checker
---
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1  = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2  = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3  = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4  = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval              :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars            :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

type Bit = Int

int2bin  :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bools  :: Int -> [[Bool]]
bools 0       = [[]]
bools (n + 1) = map (False :) bss ++ map (True :) bss
                where bss = bools n

rmdups         :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

---
--- 10.5 Abstract machine
---
data Expr = Val Int | Add Expr Expr

value          :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval'            :: Expr -> Cont -> Int
eval' (Val n) c   = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec               :: Cont -> Int -> Int
exec [] n           = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c) m  = exec c (n + m)

value'  :: Expr -> Int
value' e = eval' e []

---
--- 10.6 Class and instance declarations
---
data Shape' = Circle' Float
            | Rect' Float Float
              deriving (Eq, Ord, Show, Read)

s1 = show (Rect' 2.0 4.0)
s2 = read "Circle' 1.0" :: Shape'
