import Prelude hiding (repeat, take, replicate)

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving Show

repeat  :: a -> Tree a
repeat x = Node (repeat x) x (repeat x)

take                 :: Int -> Tree a -> Tree a
take 0 _              = Leaf
take n Leaf           = Leaf
take n (Node t1 x t2) = Node (take (n - 1) t1) x (take (n - 1) t2)

replicate  :: Int -> a -> Tree a
replicate n = take n . repeat