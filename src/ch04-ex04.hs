(&&) :: Bool -> Bool -> Bool
a && b =
  if a then
    if b then True else False
  else False