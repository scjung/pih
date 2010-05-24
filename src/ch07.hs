import Data.Char
  -- ord

---
--- 7.1 Basic concepts
---
twice    :: (a -> a) -> a -> a
twice f x = f (f x)

---
--- 7.2 Processing lists
---
map'     :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map''           :: (a -> b) -> [a] -> [b]
map'' f []       = []
map'' f (x : xs) = f x : map'' f xs

filter'     :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter''                       :: (a -> Bool) -> [a] -> [a]
filter'' p []                   = []
filter'' p (x : xs) | p x       = x : filter'' p xs
                    | otherwise = filter'' p xs

sumsqeven   :: [Int] -> Int
sumsqeven ns = sum (map' (^2) (filter' even ns))

---
--- 7.3 The foldr function
---
sum'     = foldr (+) 0
product' = foldr (*) 0
or'      = foldr (||) False
and'     = foldr (&&) True
length'  = foldr (\ _ n -> 1 + n) 0

snoc x xs = xs ++ [x]
reverse'  = foldr snoc []

---
--- 7.4 The foldl function
---
sum''     = foldl (+) 0
product'' = foldl (*) 0
or''      = foldl (||) False
and''     = foldl (&&) True
length''  = foldl (\ _ n -> 1 + n) 0
reverse'' = foldl (\ xs x -> x : xs) []

---
--- 7.5 The composition operator
---
sumsqeven' = sum . map (^2) . filter even

compose :: [a -> a] -> a -> a
compose  = foldr (.) id

---
--- 7.6 String transmitter
---
type Bit = Int

bin2int :: [Bit] -> Int
bin2int  = foldr (\x y -> x + 2 * y) 0

int2bin  :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8     :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode  = concat . map (make8 . int2bin . ord)

chop8     :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode  = map (chr . bin2int) . chop8

transmit :: String -> String
transmit  = decode . channel . encode

channel :: [Bit] -> [Bit]
channel  = id