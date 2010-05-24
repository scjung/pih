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

char  :: Char -> Parser Char
char x = sat (== x)

string         :: String -> Parser String
string []       = return []
string (x : xs) = do char x
                     string xs
                     return (x : xs)

---
--- Exercise 8.2
---
comment :: Parser ()
comment  = do string "--"
              comment1

comment1 :: Parser ()
comment1  = do x <- item
               if x == '\n' then return () else comment1
