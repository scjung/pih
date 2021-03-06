---
--- 11.2 Formalising the problem
---
data Op = Add | Sub | Mul | Div
        deriving Show

valid        :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply        :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr
          deriving Show

values            :: Expr -> [Int]
values (Val n)     = [n]
values (App o l r) = values l ++ values r

eval            :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- subsequences
subs         :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss
                where yss = subs xs

interleave           :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

-- permutations
perms         :: [a] -> [[a]]
perms []       = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

-- all permutations of all subsequences
choices   :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

---
--- 11.3 Brute force solution
---
split         :: [a] -> [([a], [a])]
split []       = []
split [_]      = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs    :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                 l <- exprs ls,
                 r <- exprs rs,
                 e <- combine l r]

combine    :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops  = [Add, Sub, Mul, Div]

solutions     :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]

putOp    :: Op -> IO ()
putOp Add = putChar '+'
putOp Sub = putChar '-'
putOp Mul = putChar '*'
putOp Div = putChar '/'

putExpr               :: Expr -> IO ()
putExpr (Val x)        = putStr (show x)
putExpr (App op el er) = do case el of
                              Val x -> putStr (show x)
                              _     -> do putChar '('
                                          putExpr el
                                          putChar ')'
                            putChar ' '
                            putOp op
                            putChar ' '
                            case er of
                              Val x -> putStr (show x)
                              _     -> do putChar '('
                                          putExpr er
                                          putChar ')'

putExprs         :: [Expr] -> IO ()
putExprs []       = return ()
putExprs (e : es) = do putExpr e
                       putChar '\n'
                       putExprs es

---
--- 11.4 Combining generation and evaluation
---
type Result = (Expr, Int)

results    :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                     lx <- results ls,
                     ry <- results rs,
                     res <- combine' lx ry]

combine'              :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions'     :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e, m) <- results ns',
                       m == n]

---
--- 11.5 Exploiting algebraic properties
---
valid'        :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

combine''              :: Result -> Result -> [Result]
combine'' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions''     :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns,
                        (e, m) <- results ns',
                        m == n]
