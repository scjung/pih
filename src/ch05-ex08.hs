import Data.Char

positions     :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

lowers   :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

alphas   :: String -> Int
alphas xs = length [x | x <- xs, isAlpha x]

count     :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

let2int  :: Char -> Int
let2int c | isLower c = ord c - ord 'a' + 26
          | otherwise = ord c - ord 'A'

int2let  :: Int -> Char
int2let n | n > 25    = chr (ord 'a' + n - 26)
          | otherwise = chr (ord 'A' + n)

shift                :: Int -> Char -> Char
shift n c | isAlpha c = int2let ((let2int c + n) `mod` 52)
          | otherwise = c

encode     :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table  = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
          6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1,
          8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
          6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent    :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs   :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['A'..'Z'] ++ ['a'..'z']]
           where n = alphas xs

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
