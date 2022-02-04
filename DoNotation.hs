myFunction = 
    do  
        x1 <- action1
        x2 <- action2
        mk_action3 x1 x2


myFunction' = 
    action1 >>=
        (\x1 -> action2 >>=
            (\x2 -> mk_action3 x1 x2))


action1 :: Maybe Int
action1 = Just 3
--action1 = [3, 4, 5]

action2 :: Maybe Int
action2 = Just 4
--action2 = [4, 5, 6]

mk_action3 :: Int -> Int -> Maybe Int
mk_action3 x y = Just (x + y)



{-

    action1 >>=
        (\x1 -> action2 >>=
            (\x2 -> mk_action3 x1 x2))


    Nothing >>=
        (\x1 -> Norhing >>=
            (\x2 -> x1 + x2))
    






-}
