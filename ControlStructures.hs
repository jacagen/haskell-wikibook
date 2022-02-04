fakeIf :: Bool -> a -> a -> a
fakeIf cond t e = case cond of
    True -> t
    False -> e

main' = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn (
        case name of 
            "Simon" -> "Haskell is a great programming language"
            "John"  -> "Haskell is a great programming language"
            "Phil"  -> "Haskell is a great programming language"
            "Koen"  -> "Debugging Haskell is fun"
            _       -> "I do not know you"
        )

main =
 do x <- getX
    putStrLn x

getX =
 do return "My Shangri-La"
    return "beneath"
    return "the summer moon"
    return "I will"
    return "return"
    return "again"
