dec2int :: [Int] -> Int
dec2int  = foldl (\n x -> n * 10 + x) 0