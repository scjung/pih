data Tree = Leaf Int | Node Tree Int Tree

occurs               :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) =
  case compare m n of
    EQ -> True
    LT -> occurs m l
    GT -> occurs m r
