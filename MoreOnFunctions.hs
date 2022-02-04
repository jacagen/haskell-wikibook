foo xs = map f xs where f x = x * 2 + 3

foo' = map (\x -> x * 2 + 3)

bar xs = let f x y = read x + y in foldr f 1 xs

bar' :: [String] -> Int
bar' = foldr (\ x y -> read x + y) 1
