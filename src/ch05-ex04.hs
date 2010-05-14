factors  :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects  :: Int -> [Int]
perfects n = [x | x <- [1..n], sumFactors x == x]
             where
               sumFactors n = sum [f | f <- factors n, f /= n]