module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

type Pos = (Int, Int)

seqn         :: [IO a] -> IO ()
seqn []       = return ()
seqn (a : as) = do a
                   seqn as

width :: Int
width  = 5

height :: Int
height  = 5

type Board = [Pos]

glider :: Board
glider  = [(4,2), (2,3), (4,3), (3,4), (4,4)]

isAlive    :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty    :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs       :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x-1, y-1), (x, y-1),
                           (x+1, y-1), (x-1, y),
                           (x+1, y), (x-1, y+1),
                           (x, y+1), (x+1, y+1)]

wrap       :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

liveneighbs  :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors  :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births  :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups         :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

nextgen  :: Board -> Board
nextgen b = survivors b ++ births b

showcells        :: GladeXML -> Board -> Board -> IO ()
showcells xml prevb b = seqn ([toggle xml p | p <- bc ++ dc])
  where bc = [p | p <- b, notElem p prevb]
        dc = [p | p <- prevb, notElem p b]

getLabel                 :: GladeXML -> Pos -> [String] -> IO (Maybe Label)
getLabel xml p []         = return Nothing
getLabel xml p (id : ids) = do tbl <- xmlGetWidget xml castToTable "table"
                               label <- xmlGetWidget xml castToLabel id
                               x <- get tbl (tableChildLeftAttach label)
                               y <- get tbl (tableChildTopAttach label)
                               if (x + 1, y + 1) == p then
                                 return (Just label)
                                 else getLabel xml p ids

toggle      :: GladeXML -> Pos -> IO ()
toggle xml p = do tbl <- xmlGetWidget xml castToTable "table"
                  Just label <- getLabel xml p ["label" ++ (show n) | n <- [1..25]]
                  text <- labelGetText label
                  labelSetText label (if text == "O" then "" else "O")

life  :: Board -> IO ()
life b = do initGUI
            Just xml <- xmlNew "life.glade"
            window <- xmlGetWidget xml castToWindow "window"
            onDestroy window mainQuit
            table <- xmlGetWidget xml castToTable "table"
            showcells xml [] b
            widgetShowAll window
            tmhandle <- timeoutAdd (life1 xml b (nextgen b)) 500
            mainGUI

life1        :: GladeXML -> Board -> Board -> IO Bool
life1 xml prevb b = do showcells xml prevb b
                       tmhandle <- timeoutAdd (life1 xml b (nextgen b)) 500
                       return False

main = life glider