import System.IO -- To define `getCh'

type Pos = (Int, Int)

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

cls :: IO ()
cls  = putStr "\ESC[2J"

seqn         :: [IO a] -> IO ()
seqn []       = return ()
seqn (a : as) = do a
                   seqn as

goto       :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat     :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

type Board = [Int]

rows :: Int
rows  = 5

initial :: Board
initial  = [5,4,3,2,1]

row        :: Int -> String
row 0       = ""
row (n + 1) = '*' : row n

showBoard  :: Board -> IO ()
showBoard []       = return ()
showBoard (x : xs) = do putStrLn ("  " ++ row x)
                        showBoard xs

end  :: Board -> Bool
end b = and (map (== 0) b)

player      :: Bool -> String
player True  = "A"
player False = "B"

message1  :: String -> IO ()
message1 m = do writeat (1, rows + 1) "                         "
                writeat (1, rows + 1) m

message2  :: String -> IO ()
message2 m = do writeat (1, rows + 2) "                         "
                writeat (1, rows + 2) m

nextRow    :: Board -> Int -> Int
nextRow b r = if (b !! (next - 1) == 0) then nextRow b next else next
  where next = if r == rows then 1 else r + 1

prevRow    :: Board -> Int -> Int
prevRow b r = if (b !! (prev - 1) == 0) then prevRow b prev else prev
  where prev = if r == 1 then rows else r - 1

chooseRowUI    :: Board -> Int -> IO Int
chooseRowUI b r = do goto (1, r)
                     c <- getCh
                     case c of
                       'w'       -> chooseRowUI b (prevRow b r)
                       's'       -> chooseRowUI b (nextRow b r)
                       '\n'      -> return r
                       ' '       -> return r
                       otherwise -> chooseRowUI b r

chooseRow  :: Board -> IO Int
chooseRow b = do message2 "Choose a row."
                 seqn [writeat (1, r) " " | r <- [1..rows]]
                 ((chooseRowUI b) . fst . head . (filter (\(r, remains) -> remains /= 0)))
                   (zip [1..rows] b)

drawStar               :: Int -> Int -> Int -> IO Int
drawStar r remain stars =
  if remain == 0 then
    chooseStarsUI r remain stars
    else
    do writeat (remain + 2, r) " "
       writeat (stars + 1, rows + 3) "*"
       chooseStarsUI r (remain - 1) (stars + 1)

undrawStar               :: Int -> Int -> Int -> IO Int
undrawStar r remain stars =
  if stars == 0 then
    chooseStarsUI r remain stars
  else
    do writeat (remain + 3, r) "*"
       writeat (stars, rows + 3) " "
       chooseStarsUI r (remain + 1) (stars - 1)

chooseStarsUI               :: Int -> Int -> Int -> IO Int
chooseStarsUI r remain stars = do goto (remain + 3, r)
                                  c <- getCh
                                  case c of
                                    'a'       -> drawStar r remain stars
                                    ' '       -> drawStar r remain stars
                                    'd'       -> undrawStar r remain stars
                                    '\n'      -> return remain
                                    otherwise -> chooseStarsUI r remain stars

chooseStars          :: Int -> Int -> IO Int
chooseStars r remains = do message2 "Choose stars to draw:"
                           remains <- chooseStarsUI r remains 0
                           writeat (1, rows + 3) "                         "
                           return remains

choose  :: Board -> IO (Int, Int)
choose b = do r <- chooseRow b
              writeat (1, r) ">"
              remains <- chooseStars r (b !! (r - 1))
              return (r, remains)

updateBoard :: Board -> Int -> Int -> IO Board
updateBoard b r remains =
  return [if r == r' then remains else s | (r', s) <- zip [1..rows] b]

turn    :: Board -> Bool -> IO ()
turn b p = do message1 ("Player " ++ player p ++ "'s turn.")
              (r, remains) <- choose b
              b' <- updateBoard b r remains
              if end b' then
                do message1 ("Player " ++ player p ++ " lose.")
                   message2 ""
                   goto (1, rows + 2)
                else
                turn b' (not p)

nim :: IO ()
nim  = do cls
          goto (1,1)
          showBoard initial
          turn initial True