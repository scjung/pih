and'            :: [Bool] -> Bool
and' []          = True
and' (True : xs) = and' xs
and' (_ : xs)    = False

concat'         :: [[a]] -> [a]
concat'       [] = []
concat' (x : xs) = x ++ (concat' xs)

replicate'          :: Int -> a -> [a]
replicate' 0 x       = []
replicate' (n + 1) x = x : replicate' n x

(!!)               :: [a] -> Int -> a
(x : xs) !! 0       = x
(x : xs) !! (n + 1) = xs Main.!! n

elem'                       :: Eq a => a -> [a] -> Bool
elem' x []                   = False
elem' x (y : ys) | x == y    = True
                 | otherwise = elem' x ys