module StringManip where

import Data.Char

uppercase, lowercase :: String -> String 
uppercase = map toUpper 
lowercase = map toLower 

capitalize :: String -> String
capitalize x = 
    let capWord [] = []
        capWord (x:xs) = toUpper x : xs
    in unwords (map capWord (words x))

cons8 :: [Int] -> [Int]
cons8 ii = 8 : ii

myCons list thing = thing : list

headTail :: [a] -> (a, [a])
headTail aa = (head aa, tail aa)

fifth :: [a] -> a
--fifth aa = head (tail (tail (tail (tail aa))))
fifth = head . tail . tail . tail . tail

h :: Int -> a -> b -> Char 
h x y z = chr (x - 2)