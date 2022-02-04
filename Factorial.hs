import Distribution.Simple.Utils (xargs)
factorial 0 = 1
factorial n = n * factorial (n - 1)

doublefactorial 0 = 1
doublefactorial 1 = 1
doublefactorial n = n * doublefactorial (n - 2)

power _ 0 = 1
power x y
    | y < 0 = 1 / power x (-y)
    | otherwise = x * power x (y - 1)

plus1 x = x + 1

addition x 0 = x
addition x y = addition (plus1 x) (y - 1)

log2 :: (Num p, Integral t) => t -> p
log2 1 = 0
log2 x = 1 + log2 (x `div` 2)

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' x a = a : replicate (x - 1) a

(!!!) :: [a] -> Int -> a
(a:aa) !!! 0 = a
(_:aa) !!! i = aa !!! (i - 1)
[] !!! _ = error "Out of bounds"

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:aa) (b:bb) = (a, b) : zip' aa bb

length' :: [a] -> Int 
length' a = go a 0 
    where
        go [] x = x
        go (_:aa) x = go aa (x + 1)
