fibs :: [Integer]
fibs  = 0 : 1 : (map (\(x, y) -> x + y) (zip fibs (tail fibs)))