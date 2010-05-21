map f    = foldr ((:) . f) []
filter p = foldr (\x xs -> if p x then x : xs else xs) []
