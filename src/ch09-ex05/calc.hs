module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Data.Char  -- isDigit, isLower, ...

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

type Pos = (Int, Int)

delete      :: Entry -> IO ()
delete entry = do text <- get entry entryText
                  set entry [ entryText := if text == [] then [] else init text ]

clear      :: Entry -> IO ()
clear entry = do set entry [ entryText := "" ]

eval   :: Entry -> Label -> IO ()
eval entry label =
  do text <- get entry entryText
     case parse expr text of
       [(n, "")]     -> do set entry [ entryText := show n ]
                           set label [ labelText := "" ]
       [(_, remain)] -> set label [ labelText := "Error: " ++ remain ]

press        :: Char -> Entry -> IO ()
press c entry = do text <- get entry entryText
                   set entry [ entryText := text ++ [c] ]

main = do initGUI
          Just xml <- xmlNew "calc.glade"
          window <- xmlGetWidget xml castToWindow "window"
          onDestroy window mainQuit
          btnQuit <- xmlGetWidget xml castToButton "btnQuit"
          onClicked btnQuit $ do widgetDestroy window
          entry <- xmlGetWidget xml castToEntry "entry"
          btnClear <- xmlGetWidget xml castToButton "btnClear"
          onClicked btnClear $ do clear entry
          btnDelete <- xmlGetWidget xml castToButton "btnDelete"
          onClicked btnDelete $ do delete entry
          label <- xmlGetWidget xml castToLabel "label"
          btn1 <- xmlGetWidget xml castToButton "btn1"
          onClicked btn1 $ do press '1' entry
          btn2 <- xmlGetWidget xml castToButton "btn2"
          onClicked btn2 $ do press '2' entry
          btn3 <- xmlGetWidget xml castToButton "btn3"
          onClicked btn3 $ do press '3' entry
          btn4 <- xmlGetWidget xml castToButton "btn4"
          onClicked btn4 $ do press '4' entry
          btn5 <- xmlGetWidget xml castToButton "btn5"
          onClicked btn5 $ do press '5' entry
          btn6 <- xmlGetWidget xml castToButton "btn6"
          onClicked btn6 $ do press '6' entry
          btn7 <- xmlGetWidget xml castToButton "btn7"
          onClicked btn7 $ do press '7' entry
          btn8 <- xmlGetWidget xml castToButton "btn8"
          onClicked btn8 $ do press '8' entry
          btn9 <- xmlGetWidget xml castToButton "btn9"
          onClicked btn9 $ do press '9' entry
          btnAdd <- xmlGetWidget xml castToButton "btnAdd"
          onClicked btnAdd $ do press '+' entry
          btnSub <- xmlGetWidget xml castToButton "btnSub"
          onClicked btnSub $ do press '-' entry
          btnMul <- xmlGetWidget xml castToButton "btnMul"
          onClicked btnMul $ do press '*' entry
          btnDiv <- xmlGetWidget xml castToButton "btnDiv"
          onClicked btnDiv $ do press '/' entry
          btnLParen <- xmlGetWidget xml castToButton "btnLParen"
          onClicked btnLParen $ do press '(' entry
          btnRParen <- xmlGetWidget xml castToButton "btnRParen"
          onClicked btnRParen $ do press ')' entry
          btnAnswer <- xmlGetWidget xml castToButton "btnAnswer"
          onClicked btnAnswer $ do eval entry label
          widgetShowAll window
          mainGUI