import Data.Char

type Bit = Int

unfold              :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

bin2int :: [Bit] -> Int
bin2int  = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin  = unfold (== 0) (`mod` 2) (`div` 2)

make8     :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity  = (`mod` 2) . length . filter (== 1)

addParity     :: [Bit] -> [Bit]
addParity bits = parity bits : bits

encode :: String -> [Bit]
encode  = concat . map (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9  = unfold null (take 9) (drop 9)

dataOfChunk                                :: [Bit] -> [Bit]
dataOfChunk (pb : bits) | parity bits == pb = bits
                        | otherwise         = error "incorrect parity bit"

decode :: [Bit] -> String
decode  = map (chr . bin2int . dataOfChunk) . chop9

transmit :: String -> String
transmit  = decode . channel . encode

channel :: [Bit] -> [Bit]
channel  = id

erroneousChannel :: [Bit] -> [Bit]
erroneousChannel  = tail

erroneousTransmit :: String -> String
erroneousTransmit   = decode . erroneousChannel . encode
