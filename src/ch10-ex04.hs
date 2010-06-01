data Tree = Leaf Int
          | Node Tree Tree
            deriving Show

split   :: [a] -> ([a], [a])
split xs = (take n xs, drop n xs)
           where n = length xs `div` 2

balance :: [Int] -> Tree
balance [n] = Leaf n
balance xs  = Node (balance l) (balance r)
              where (l, r) = split xs
