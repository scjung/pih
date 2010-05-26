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

life :: Board -> IO ()
life  b = do cls
             life1 [] b

life1        :: Board -> Board -> IO ()
life1 prevb b = do showcells prevb b
                   goto (1, height + 1) -- move the cursor out of the board
                   wait 500000
                   life1 b (nextgen b)

wait  :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]