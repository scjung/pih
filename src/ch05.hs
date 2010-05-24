import Data.Char

---
--- 5.1 Generators
---

concat    :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts   :: [(a, b)] -> [a]
firsts ps = [a | (a, _) <- ps]

length   :: [a] -> Int
length xs = sum [1 | _ <- xs]

---
--- 5.2 Guards
---

factors  :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime  :: Int -> Bool
prime n = factors n == [1, n]
-- Note that deciding that a number such as 15 is not prime does not require
-- the function prime to produce all of its factors, because under lazy
-- evaluation the result False is returned as soon as any factor other than
-- one or the number itself is produced.

find    :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

---
--- 5.3 The zip function
---

pairs   :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted   :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]
-- Similarly to the function prime, deciding that a list such as [1,3,2,4]
-- is not sorted may not require the function sorted to produce all pairs
-- of adjacent elements, because the result False is returned as sson as
-- any non-ordered pair is produced.

positions     :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = Prelude.length xs - 1

---
--- 5.4 String comprehensions
---

lowers   :: String -> Int
lowers xs = Prelude.length [x | x <- xs, isLower x]

count     :: Char -> String -> Int
count x xs = Prelude.length [x' | x' <- xs, x == x']

---
--- 5.5 The Caesar ciper
---

let2int  :: Char -> Int
let2int c = ord c - ord 'a'

int2let  :: Int -> Char
int2let n = chr (ord 'a' + n)

shift                :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode     :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table  = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
          6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent    :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs   :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

chisqr      :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate     :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack   :: String -> String
crack xs = encode (-factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table | n <- [0..25]]
             table' = freqs xs
