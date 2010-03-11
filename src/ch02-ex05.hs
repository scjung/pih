init1 :: [a] -> [a]
init1 xs = reverse (tail (reverse xs))

init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs