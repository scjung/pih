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

alphanum :: Parser Char
alphanum  = sat isAlphaNum

char  :: Char -> Parser Char
char x = sat (== x)

many  :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1  :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v : vs)

nat :: Parser Int
nat  = do xs <- many1 digit
          return (read xs)

---
--- Exercise 8.1
---
int :: Parser Int
int  = do char '-'
          n <- nat
          return (-n)
         +++ nat
