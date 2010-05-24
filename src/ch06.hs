---
--- 6.1 Basic concepts
---

factorial        :: Int -> Int
factorial 0       = 1
factorial (n + 1) = (n + 1) * factorial n


---
--- 6.2 Recursion on lists
---

length'         :: [a] -> Int
length' []       = 0
length' (_ : xs) = 1 + length xs

reverse'         :: [a] -> [a]
reverse' []       = []
reverse' (x : xs) = reverse xs ++ [x]

insert                       :: Ord a => a -> [a] -> [a]
insert x []                   = [x]
insert x (y : ys) | x <= y    = x : y : ys
                  | otherwise = y : insert x ys

isort         :: Ord a => [a] -> [a]
isort []       = []
isort (x : xs) = insert x (isort xs)

---
--- 6.3 Multiple arguments
---

zip'                  :: [a] -> [b] -> [(a, b)]
zip' [] _              = []
zip' _ []              = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

drop'                 :: Int -> [a] -> [a]
drop' 0 xs             = xs
drop' (n + 1) []       = []
drop' (n + 1) (x : xs) = drop' n xs

---
--- 6.4 Multiple recursion
---

fibonacci        :: Int -> Int
fibonacci 0       = 0
fibonacci 1       = 0
fibonacci (n + 2) = fibonacci n + fibonacci (n + 1)

qsort         :: Ord a => [a] -> [a]
qsort []       = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                 where
                   smaller = [a | a <- xs, a <= x]
                   larger = [a | a <- xs, a > x]

---
--- 6.5 Mutual recursion
---

even'        :: Int -> Bool
even' 0       = True
even' (n + 1) = odd' n

odd'        :: Int -> Bool
odd' 0       = False
odd' (n + 1) = even' n

evens         :: [a] -> [a]
evens []       = []
evens (x : xs) = x : odds xs

odds         :: [a] -> [a]
odds []       = []
odds (_ : xs) = evens xs