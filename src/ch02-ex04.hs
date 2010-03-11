last1 :: [a] -> a
last1 xs = head (reverse xs)

last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

last3 :: [a] -> a
last3 xs = head (drop (length xs - 1) xs)