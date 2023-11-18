ehlogico :: Bool->Bool->Bool
ehlogico v1 v2
            |v1 == True && v2 == True = True
            |otherwise = False

ehlogico2 :: Bool->Bool->Bool
ehlogico2 True True = True
ehlogico2 True False = True
ehlogico2 False True = True
ehlogico2 False False = True

ehlogico3 :: Bool->Bool->Bool
ehlogico3 v1 v2
                |v1 == True && v2 == True = True
                |v1 == True && v2 == False = False
                |v1 == False && v2 == True = False
                |otherwise = False

ehlogico4 :: (Bool, Bool)->Bool
ehlogico4 x
            |fst(x) == True && snd(x) == True = True
            |otherwise = False

equivale :: Bool->Bool->Bool
equivale True True = True
equivale True False = False
equivale False True = False
equivale False False = True

equivale2 :: Bool->Bool->Bool
equivale2 x y
            |x==y = True
            |otherwise = False

calcula :: Char->Float->Float->Float
calcula x y z
              |x == '*' = y * z
              |x == '/' = y / z
              |otherwise = error "operador não permitido para x"



