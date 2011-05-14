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

subs         :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss
                where yss = subs xs

interleave           :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms         :: [a] -> [[a]]
perms []       = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices   :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

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

count   :: [Int] -> (Int, Int)
count ns = (length chosen, length result)
  where chosen = [e | ns' <- choices ns, e <- exprs ns']
        result = [eval e | e <- chosen]

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
