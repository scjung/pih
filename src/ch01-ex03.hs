import Prelude hiding (product)

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs