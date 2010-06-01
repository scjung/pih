import Data.Char

type Assoc k v = [(k, v)]

find    :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop
          deriving Show

type Subst = Assoc Char Bool

eval              :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars            :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

type Bit = Int

int2bin  :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bools  :: Int -> [[Bool]]
bools 0       = [[]]
bools (n + 1) = map (False :) bss ++ map (True :) bss
                where bss = bools n

rmdups         :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

data Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
  p >>= f  = Parser (\inp -> case parse p inp of
                               []         -> []
                               [(v, out)] -> parse (f v) out)
  return v = Parser (\inp -> [(v, inp)])

failure :: Parser a
failure  = Parser (\inp -> [])

item :: Parser Char
item  = Parser (\inp -> case inp of
                          []       -> []
                          (x : xs) -> [(x, xs)])

parse               :: Parser a -> String -> [(a, String)]
parse (Parser p) inp = p inp

(+++)  :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v, out)] -> [(v, out)])

sat  :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure


char  :: Char -> Parser Char
char x = sat (== x)

string         :: String -> Parser String
string []       = return []
string (x : xs) = do char x
                     string xs
                     return (x : xs)

lower :: Parser Char
lower  = sat isLower

alphanum :: Parser Char
alphanum  = sat isAlphaNum

space :: Parser ()
space  = do many (sat isSpace)
            return ()

token  :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

symbol   :: String -> Parser String
symbol xs = token (string xs)

many  :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1  :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v : vs)

symAnd :: Parser String
symAnd  = symbol "/\\"

symOr :: Parser String
symOr  = symbol "\\/"

symNot :: Parser String
symNot  = symbol "!"

symTrue :: Parser String
symTrue  = symbol "T"

symFalse :: Parser String
symFalse  = symbol "F"

symImply :: Parser String
symImply  = symbol "=>"

symEquiv :: Parser String
symEquiv  = symbol "<=>"

prop :: Parser Prop
prop  = do pl <- term
           do symImply
              pr <- prop
              return (Imply pl pr)
             +++ do symEquiv
                    pr <- prop
                    return (Equiv pl pr)
          +++ term

term :: Parser Prop
term  = do pl <- pNot
           do symAnd
              pr <- term
              return (And pl pr)
             +++ do symOr
                    pr <- term
                    return (Or pl pr)
          +++ pNot

pNot :: Parser Prop
pNot  = do symNot
           p <- pNot
           return (Not p)
          +++ paren

paren :: Parser Prop
paren  = do symbol "("
            p <- prop
            symbol ")"
            return p
           +++ pConst
           +++ var

pConst :: Parser Prop
pConst  = do symbol "T"
             return (Const True)
            +++ do symbol "F"
                   return (Const False)

var :: Parser Prop
var  = do x <- lower
          return (Var x)

process    :: String -> IO ()
process []  = return ()
process inp =
  do case parse prop inp of
       [(p, [])] -> putStrLn (if isTaut p then "yes" else "no")
       otherwise -> putStrLn "syntax error."
     taut

taut :: IO ()
taut  = do putStr "tautology? "
           inp <- getLine
           process inp
