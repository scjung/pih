import Data.Char

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

digit :: Parser Char
digit  = sat isDigit

lower :: Parser Char
lower  = sat isLower

upper :: Parser Char
upper  = sat isUpper

letter :: Parser Char
letter  = sat isLetter

alphanum :: Parser Char
alphanum  = sat isAlphaNum

char  :: Char -> Parser Char
char x = sat (== x)

string         :: String -> Parser String
string []       = return []
string (x : xs) = do char x
                     string xs
                     return (x : xs)

many  :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1  :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v : vs)

ident :: Parser String
ident  = do x <- lower
            xs <- many alphanum
            return (x : xs)

nat :: Parser Int
nat  = do xs <- many1 digit
          return (read xs)

space :: Parser ()
space  = do many (sat isSpace)
            return ()

token  :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier  = token ident

natural :: Parser Int
natural  = token nat

symbol   :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Int
expr  = do t <- term
           do symbol "+"
              e <- expr
              return (t + e)
             +++ do symbol "-"
                    e <- expr
                    return (t - e)
             +++ return t

term :: Parser Int
term  = do e <- expo
           do symbol "*"
              t <- term
              return (e * t)
             +++ do symbol "/"
                    t <- term
                    return (e `div` t)
             +++ return e

expo :: Parser Int
expo  = do f <- factor
           do symbol "^"
              e <- expo
              return (f ^ e)
             +++ return f

factor :: Parser Int
factor  = do symbol "("
             e <- expr
             symbol ")"
             return e
            +++ natural

eval   :: String -> Int
eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("unused input: " ++ out)
            [] -> error "invalid input"