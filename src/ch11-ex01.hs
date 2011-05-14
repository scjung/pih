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

choices'   :: [a] -> [[a]]
choices' xs = [y | x <- (subs xs), y <- perms x]