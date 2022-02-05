{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use zipWith" #-}

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random
import Debug.Trace

newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b)}

instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = dot
        where
            (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
                let (cir1', b) = cir1 a
                    (cir2', c) = cir2 b
                in (cir2' `dot` cir1', c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(b, d) ->
        let (cir', c) = cir b
        in (first cir', (c, d))

runCircuit :: Circuit a b -> [a] -> [b]
{-runCircuit  _   []      = []
runCircuit  cir (x:xs)  =
    let (cir', x') = unCircuit cir x
    in x' : runCircuit cir' xs
-}
runCircuit cir inputs =
    snd $ mapAccumL unCircuit cir inputs

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
    let (output, acc') = input `f` acc
    in (accum acc' f, output)

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))

total :: Num a => Circuit a a
total = accum' 0 (+)

mean1 :: (Show a, Fractional a) => Circuit a a
-- mean1 = accum (0, 0) (\a (count, sum) -> --trace ("sum: " ++ show sum ++ " count: " ++ show count ++ " a: " ++ show a ++ "\n") 
--    ((sum + a) / (count + 1), (count + 1, sum + a)))
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
    t <- total -< value
    n <- total -< 1
    returnA -< t / n

generator :: Random a => (a, a) -> StdGen -> Circuit () a
generator range sg = accum sg (\_ acc -> randomR range acc)

dictionary = ["dog", "cat", "bird"]

pickWord :: StdGen -> Circuit () String
pickWord sg = proc _ -> do
    r <- generator (0, length dictionary - 1) sg -< ()
    returnA -< dictionary !! r

oneShot :: Circuit () Bool
oneShot = accum True $ \_ acc -> (acc, False)

delayedEcho :: a -> Circuit a a
delayedEcho a = accum a $ flip (,)

instance ArrowChoice Circuit where
    left orig@(Circuit cir) = Circuit $ \ebd -> case ebd of
        Left b -> let (cir', c) = cir b
                    in (left cir', Left c)
        Right d -> (left orig, Right d)

getWord :: StdGen -> Circuit () String
getWord rng = proc () -> do
    -- If this is the first game loop, run pickWord. mPicked becomes Just <word>.
    -- On subsequent loops, mPicked is Nothing.
    firstTime <- oneShot -< ()
    mPicked <- if firstTime
                then do
                    picked <- pickWord rng -< ()
                    returnA -< Just picked
                else returnA -< Nothing
    -- An accumulator that retains the last 'Just' value.
    mWord <- accum' Nothing mplus -< mPicked
    returnA -< fromJust mWord


{-
data HangManState = HangManState { word :: String, guessedCorrect :: [Char], guessedWrong :: [Char] }

hangMan :: Circuit Char String 
hangMan = proc guess -> do
    picked <- 
-}

attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung = "Lives: ["
              ++ replicate (attempts - hung) '#'
              ++ replicate hung ' '
              ++ "]"

hangman :: StdGen -> Circuit String (Bool, [String])
hangman rng = proc userInput -> do
    word <- getWord rng -< ()
    let letter = listToMaybe userInput
    guessed <- updateGuess -< (word, letter)
    hung <- updateHung -< (word, letter)
    end <- delayedEcho True -< not (word == guessed || hung >= attempts)
    let result
          | word == guessed = [guessed, "You won!"]
          | hung >= attempts = [guessed, livesLeft hung, "You died!"]
          | otherwise = [guessed, livesLeft hung]
    returnA -< (end, result)
        where
            updateGuess :: Circuit (String, Maybe Char) String
            updateGuess = accum' (repeat '_') $ \(word, letter) guess ->
                case letter of 
                    Just l -> map (\(w, g) -> if w == l then w else g) (zip word guess)
                    Nothing -> take (length word) guess

            updateHung :: Circuit (String, Maybe Char) Int
            updateHung = proc (word, letter) -> do
                total -< case letter of 
                    Just l -> if l `elem` word then 0 else 1
                    Nothing -> 0    

main :: IO ()
main = do
    rng <- getStdGen 
    interact $ unlines
        . ("Welcome to Arrow Hangman":)
        . concat . map snd . takeWhile fst
        . runCircuit (hangman rng)
        . ("":)
        . lines 
