fibs :: [Integer]
fibs  = 0 : 1 : (map (\(x, y) -> x + y) (zip fibs (tail fibs)))

fib  :: Int -> Integer
fib n = head (drop (n - 1) fibs)