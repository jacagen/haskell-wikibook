
main = do
    putStrLn "The base?"
    b <- getLine
    putStrLn "The height?"
    h <- getLine
    putStrLn ("The area of that triangle is " ++ area b h)
        where area x y = show (read x * read y / 2)

    