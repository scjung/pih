instance Monad Maybe where
  return v       = Just v
  Nothing >>= f  = Nothing
  (Just v) >>= f = f v

instance Monad [] where
  return v = [v]
  m >>= n  = foldr ((++) . n) [] m
