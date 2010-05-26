import System.IO
  -- To define `getCh'.

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

backspace :: IO ()
backspace  = do putStr "\ESC[1D" -- move back
                putStr " "       -- delete the last character
                putStr "\ESC[1D" -- move back

readLine :: IO String
readLine  = readLine1 ""

readLine1     :: String -> IO String
readLine1 prev = do x <- getCh
                    case x of
                      '\n'   -> do putChar x
                                   return (reverse prev)
                      '\DEL' -> do backspace
                                   readLine1 (tail prev)
                      _      -> do putChar x
                                   readLine1 (x : prev)
