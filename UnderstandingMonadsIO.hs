import Data.Char ( toUpper )

main = do
    putStrLn "Write your string: "
    string <- getLine
    putStrLn $ shout string

shout = map toUpper

printList :: Show s => [s] -> IO ()
printList = mapM_ $ putStrLn . show