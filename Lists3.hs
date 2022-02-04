import Data.List ( foldl' )

{-

foldr (:) [] 1:2:3:[]
(:) 1 (foldr (:) [] 2:3:[])
(:) 1 ((:) 2 (foldr (:) [] 3:[]))
(:) 1 ((:) 2 ((:) 3 (foldr (:) [] [])))
(:) 1 ((:) 2 ((:) 3 ([])))

-}

{- XXX bad example

foldl   (:) []                          1:2:3:[]
foldl   (:) ((:) [] 1)                  2:3:[]
foldl   (:) ((:) ((:) [] 1) 2)          3:[]
foldl   (:) ((:) ((:) ((:) [] 1) 2) 3)  []
((:) ((:) ((:) [] 1) 2) 3)
-}

echoesr :: [Int] -> [Int]
echoesr = foldr (\i aa -> replicate i i ++ aa) []

echoesl :: [Int] -> [Int]
echoesl = foldl (\a x -> a ++ replicate x x ) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e a -> f e : a) []

and' :: [Bool] -> Bool
and' (x:xs) = x && and' xs
and' [] = True

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

or' :: [Bool] -> Bool
or' (x:xs) = x || or' xs
or' [] = False

or'' :: [Bool] -> Bool
or'' = foldr (||) False

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max

minimum' :: Ord a => [a] -> a
minimum' = foldr1 min

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

scanrR :: (a -> b -> b) -> b -> [a] -> [b]
scanrR f acc [] = [acc]
scanrR f acc [x] = [f x acc, acc]
scanrR f acc (m:ms) =
    let a = scanrR f acc ms
    in f m (head a) : a

scanrF :: (a -> b -> b) -> b -> [a] -> [b]
scanrF f init = foldr (\e a -> f e (head a):a) [init]

scanlR :: (a -> b -> a) -> a -> [b] -> [a]
scanlR f init [] = [init]
scanlR f init (x:xs) = init : scanlR f (f init x) xs

scanlF :: (a -> b -> a) -> a -> [b] -> [a]
scanlF f init = foldl (\a e -> a ++ [f (last a) e]) [init]

factList :: Integer -> [Integer]
factList x = scanl1 (*) [1..x]

returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n xs = [x | x <- xs, mod x n == 0]

choosingTails :: [[Int]] -> [[Int]]
choosingTails ll = [tail x | x <- ll, head x > 5, not $ null x]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

doubleOfFirstForEvenSeconds :: [(Int, Int)] -> [Int]
doubleOfFirstForEvenSeconds pp =  map (\(x, y) -> 2 * x) $ filter (\(x, y) -> even y) pp