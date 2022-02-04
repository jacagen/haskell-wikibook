import Data.Char ( toLower )
import Control.Monad
quickSort' :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
-- No matter how we compare two things the base case doesn't change,
-- so we use the _ "wildcard" to ignore the comparison function.
quickSort' _ [] = []

-- c is our comparison function
quickSort' c (x : xs) = (quickSort' c less) ++ (x : equal) ++ (quickSort' c more)
    where
        less  = filter (\y -> y `c` x == LT) xs
        equal = filter (\y -> y `c` x == EQ) xs
        more  = filter (\y -> y `c` x == GT) xs

insensitive :: String -> String -> Ordering
insensitive s t = compare (Main.toLower s) (Main.toLower t)


toLower :: String -> String
toLower = map Data.Char.toLower

dictionary = ["I", "have", "a", "thing", "for", "Linux"]


for :: Show a => a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job =
    if p i
        then do
            job i
            for (f i) p f job
        else return ()


main = do
    for 1 (<10) (+1) print


mapMonad :: Monad m => (a -> m b) -> [a] -> m [b]
mapMonad f [] = return []
mapMonad f (x:xs) = do
    this <- f x
    rest <- mapMonad f xs
    return (this : rest)

sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (i:is) = do
    this <- i
    rest <- sequenceIO is
    return (this:rest)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO = mapMonad

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a,b) = f a b

const' :: a -> b -> a
const' a _ = a

{-




-}

hardFoldl :: (a -> e -> a) -> a -> [e] -> a
hardFoldl f a es = foldr (flip f) a (reverse es)

hardFoldl2 :: (a -> e -> a) -> a -> [e] -> a
hardFoldl2 f a es = 
    (foldr (flip (.)) id (map (flip f) es)) a

