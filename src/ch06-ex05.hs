merge                              :: Ord a => [a] -> [a] -> [a]
merge [] xs                         = xs
merge xs []                         = xs
merge (x : xs) (y : ys) | x <= y    = x : merge xs (y : ys)
                        | otherwise = y : merge (x : xs) ys

msort    :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
            where
              n = length xs `div` 2
              left = take n xs
              right = drop n xs
