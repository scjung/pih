--(a) using a conditional expression
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

--(b) using guarded equations
safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
safetail2 xs | otherwise = tail xs

--(c) using pattern matching
safetail3 :: [a] -> [a]
safetail3 []       = []
safetail3 (_ : xs) = xs