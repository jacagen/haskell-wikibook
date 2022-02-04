import Text.Read

interactiveDouble = do
    putStrLn "Enter a double:"
    n <- getLine
    let parsed = readMaybe n :: Maybe Double
    case parsed of
      Nothing -> putStrLn ("Could not parse \"" ++ n ++ "\"")
      Just x -> putStrLn ("Value: " ++ show (2 * x))

main = do
    putStrLn "Enter a number:"
    n1 <- readMaybe <$> getLine 
    putStrLn "Enter a second number:"
    n2 <- readMaybe <$> getLine 
    let dplus = fmap (+) n1
    let result = dplus <*> n2
    case result of      
        Nothing -> putStrLn "Invalid input"
        Just x -> putStrLn ("Result is " ++ show x)

interactiveConcatenating :: IO ()
interactiveConcatenating = do
    c <- putStrLn "Choose two strings:"  *> ((++) <$> getLine <*> getLine)
    putStrLn "Let's concatenate them:" *> putStrLn c

foo =
    (\_ y -> y) <$> putStrLn "First!" <*> putStrLn "Second!"