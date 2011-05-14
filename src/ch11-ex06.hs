import Data.List
import Data.Ord

data Op = Add | Sub | Mul | Div | Exp
        deriving Show

valid        :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && x `mod` y == 0
valid Exp x y = x >= 1 && y >= 1

apply        :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

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

type Result = (Expr, Int)

results    :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                     lx <- results ls,
                     ry <- results rs,
                     res <- combine lx ry]

combine              :: Result -> Result -> [Result]
combine (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]


ops :: [Op]
ops  = [Add, Sub, Mul, Div, Exp]

solutions     :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      (e, m) <- results ns',
                      m == n]

nearests     :: Int -> [(Expr, Int)] -> [Expr]
nearests n es = [e | (e, d) <- exprsAndDiffs, d == minDiff]
  where exprsAndDiffs = map (\(e, m) -> (e, abs (n - m))) es
        minDiff = (minimum . snd . unzip) exprsAndDiffs

solutionsOrNearests     :: [Int] -> Int -> [Expr]
solutionsOrNearests ns n =
  case exact of
    [] -> nearests n res
    _  -> exact
    where res = [(e, m) | ns' <- choices ns, (e, m) <- results ns']
          exact = [e | (e, m) <- res, m == n]

putOp    :: Op -> IO ()
putOp Add = putChar '+'
putOp Sub = putChar '-'
putOp Mul = putChar '*'
putOp Div = putChar '/'
putOp Exp = putChar '^'

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

solve            :: Expr -> Int
solve (Val n)     = n
solve (App o l r) = apply o (solve l) (solve r)

putSolutions         :: [Expr] -> IO ()
putSolutions []       = return ()
putSolutions (e : es) = do putExpr e
                           putStr " = "
                           putStr (show (solve e))
                           putChar '\n'
                           putSolutions es

simplicity :: Expr -> Int
simplicity  = length . values

sortExprs :: [Expr] -> [Expr]
sortExprs  = Data.List.sortBy (Data.Ord.comparing simplicity)

sortedSolutionsOrNearests     :: [Int] -> Int -> [Expr]
sortedSolutionsOrNearests ns n =
  case exact of
    [] -> sortExprs (nearests n res)
    _  -> sortExprs exact
    where res = [(e, m) | ns' <- choices ns, (e, m) <- results ns']
          exact = [e | (e, m) <- res, m == n]

