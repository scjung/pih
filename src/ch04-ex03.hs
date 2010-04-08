-- ~모든 경우를 나열해 봄~
or1 :: Bool -> Bool -> Bool
or1 True True   = True
or1 True False  = True
or1 False True  = True
or1 False False = False

-- ~두 인자 중 하나라도 True이면 결과는 True~
or2 :: Bool -> Bool -> Bool
or2 True _ = True
or2 _ True = True
or2 _      = False

-- ~두 인자 모두 False이면 결과는 False~
or3 :: Bool -> Bool -> Bool
or3 False False = False
or3 _ _         = True

-- ~첫번째 인자가 False이면 결과는 두번째 인자와 동일~
or4 :: Bool -> Bool -> Bool
or4 False b = b
or4 True _  = True
