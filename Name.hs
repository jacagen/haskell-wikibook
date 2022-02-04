main = do
    putStrLn "What is your name?"
    name <- getLine
    if name == "Simon" ||  name == "John" || name ==  "Phil"
        then putStrLn "Haskell is a great programming language"
        else if name == "Koen"
            then putStrLn "Debugging Haskell is fun"
            else putStrLn "I do not know you"