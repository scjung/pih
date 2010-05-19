(^)        :: Int -> Int -> Int
m ^ 0       = 1
m ^ (n + 1) = m * (m Main.^ n)