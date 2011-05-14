remove           :: Eq a => a -> [a] -> [a]
remove _ []       = []
remove n (x : xs)
  | n == x        = xs
  | otherwise     = x : remove n xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _              = True
isChoice _ []              = False
isChoice (x : xs) (y : ys)
  | x == y                 = isChoice xs ys
  | otherwise              = elem x ys && isChoice xs (y : remove x ys)

