inf :: Int
inf  = 1 + inf

ones :: [Int]
ones  = 1 : ones

primes :: [Int]
primes  = sieve [2..]

sieve         :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

sumwith           :: Int -> [Int] -> Int
sumwith v []       = v
sumwith v (x : xs) = sumwith (v + x) xs

sumwith'           :: Int -> [Int] -> Int
sumwith' v []       = v
sumwith' v (x : xs) = (sumwith' $! (v + x)) xs