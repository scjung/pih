all  :: (a -> Bool) -> [a] -> Bool
all p = and . (map p)

any  :: (a -> Bool) -> [a] -> Bool
any p = or . (map p)

takeWhile'                       :: (a -> Bool) -> [a] -> [a]
takeWhile' p []                   = []
takeWhile' p (x : xs) | p x       = x : takeWhile' p xs
                      | otherwise = []

dropWhile'                       :: (a -> Bool) -> [a] -> [a]
dropWhile' p []                   = []
dropWhile' p (x : xs) | p x       = dropWhile' p xs
                      | otherwise = x : xs