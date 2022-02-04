import Data.List

takeInt :: Num a => Int -> [a] -> [a]
takeInt 0 _ = []
takeInt _ [] = error "Out of bounds"
takeInt i (a:aa) = a:takeInt (i - 1) aa

dropInt :: Num a => Int -> [a] -> [a]
dropInt 0 aa = aa
dropInt _ [] = error "Out of bounds"
dropInt i (_:aa) = dropInt (i - 1) aa

sumInt :: Num a => [a] -> a
sumInt [] = 0
sumInt (a:aa) = a + sumInt aa

scanSum :: Num a => [a] -> [a]
scanSum aa = doSum 0 aa
    where
        doSum _ [] = []
        doSum x (a:aa) = (x + a) : doSum (x + a) aa

diffs :: Num a => [a] -> [a]
diffs (a:b:cc) = (b - a) : diffs (b:cc)
diffs _ = []

negations :: [Int] -> [Int]
negations = map ((-1) *)

divisors p = [ f | f <- [1..p], p `mod` f == 0]

xss :: [Int] -> [[Int]]
xss = map divisors

negXss :: [Int] -> [[Int]]
negXss = map negations . xss

rle :: Eq a => [a] -> [(Int, a)]
rle = map (pairify length head) . group
    where pairify ab ac a = (ab a, ac a)

rld :: [(Int, a)] -> [a]
rld = concatMap f
    where
        f (i, x) = replicate i x

last' :: [a] -> a
last' [x] = x
last' (_:xx) = last' xx
last' [] = error "Out of bounds"

init' :: [a] -> [a]
init' [x] = []
init' (x:xx) = x : init xx
init' [] = []
