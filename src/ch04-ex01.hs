halve :: [a] -> ([a], [a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)