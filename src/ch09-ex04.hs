import System.IO  -- getCh

cls :: IO ()
cls  = putStr "\ESC[2J"

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

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

width :: Int
width  = 5

height :: Int
height  = 5

type Board = [Pos]

glider :: Board
glider  = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells        :: Board -> Board -> IO ()
showcells prevb b = seqn ([writeat p "o" | p <- bc] ++ [writeat p " " | p <- dc])
                    where bc = [p | p <- b, notElem p prevb]
                          dc = [p | p <- prevb, notElem p b]

process            :: Char -> Board -> Pos -> IO Board
process c b p
  | elem c "qQ\ESC" = quit b
  | c == 'a'        = move b p (-1) 0
  | c == 'd'        = move b p 1 0
  | c == 'w'        = move b p 0 (-1)
  | c == 's'        = move b p 0 1
  | elem c " \n"    = toggle b p
  | otherwise       = boardEditor b p

quit  :: Board -> IO Board
quit b = do goto (1, height + 1)
            return b

move  :: Board -> Pos -> Int -> Int -> IO Board
move b (x,y) ix iy = do goto (x', y')
                        boardEditor b (x', y')
                          where x' = (x - 1 + ix) `mod` width + 1
                                y' = (y - 1 + iy) `mod` height + 1

toggle                :: Board -> Pos -> IO Board
toggle b p | elem p b  = do writeat p " "
                            boardEditor (filter (/= p) b) p
           | otherwise = do writeat p "o"
                            boardEditor (p : b) p

boardEditor  :: Board -> Pos -> IO Board
boardEditor b p = do goto p
                     c <- getCh
                     process c b p

modifyBoard  :: Board -> IO Board
modifyBoard b = do cls
                   showcells [] b
                   boardEditor b (1,1)

createBoard :: IO Board
createBoard  = modifyBoard []