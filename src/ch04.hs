abs1 :: Int -> Int
abs1 n = if n >= 0 then n else -n

abs2 :: Int -> Int
abs2 n | n >= 0    = n
       | otherwise = -n

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

test :: [Char] -> Bool
test ('a' : _) = True
test _         = False

pred :: Int -> Int
pred 0       = 0
pred (n + 1) = n
pred _       = 0

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0 .. n-1]

and :: [Bool] -> Bool
and = foldr (&&) True