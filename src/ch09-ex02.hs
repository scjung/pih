import Data.Char  -- isDigit, isLower, ...
import System.IO  -- getCh

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
term  = do f <- factor
           do symbol "*"
              t <- term
              return (f * t)
             +++ do symbol "/"
                    t <- term
                    return (f `div` t)
             +++ return f

factor :: Parser Int
factor  = do symbol "("
             e <- expr
             symbol ")"
             return e
            +++ natural

beep :: IO ()
beep  = putStr "\BEL"

cls :: IO ()
cls  = putStr "\ESC[2J"

type Pos = (Int, Int)

goto       :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat     :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn         :: [IO a] -> IO ()
seqn []       = return ()
seqn (a : as) = do a
                   seqn as

putStr'' xs = seqn [putChar x | x <- xs]

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

box :: [String]
box  = ["+---------------+",
        "|               |",
        "+---+---+---+---+",
        "| q | c | d | = |",
        "+---+---+---+---+",
        "| 1 | 2 | 3 | + |",
        "+---+---+---+---+",
        "| 4 | 5 | 6 | - |",
        "+---+---+---+---+",
        "| 7 | 8 | 9 | * |",
        "+---+---+---+---+",
        "| 0 | ( | ) | / |",
        "+---+---+---+---+"]

buttons :: [Char]
buttons  = standard ++ extra
           where
             standard = "qcd=123+456-789*0()/"
             extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox  = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display   :: String -> IO ()
display xs = do writeat (3,2) "             "
                writeat (3,2) (reverse (take 13 (reverse xs)))

-- Show an error indicator of the nth character.
showError  :: Int -> IO ()
showError n = writeat (n + 2, 1) "v"

-- Hide error indicators
hideError :: IO ()
hideError  = do writeat (3,1) "-------------"

calc   :: String -> IO ()
calc xs = do display xs
             c <- getCh
             hideError
             if elem c buttons then
               process c xs
               else
               do beep
                  calc xs

process            :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC"    = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "cC"        = clear
  | elem c "=\n"       = eval xs
  | otherwise          = press c xs

quit :: IO ()
quit  = goto (1, 14)

delete   :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

clear :: IO ()
clear  = calc ""

eval   :: String -> IO ()
eval xs = case parse expr xs of
            [(n, "")]     -> calc (show n)
            [(_, remain)] -> do beep
                                showError (length xs - length remain + 1)
                                calc xs

press     :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run  = do cls
          showbox
          clear
