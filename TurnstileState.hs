{-# LANGUAGE TupleSections #-}
module TurnstileState (TurnstileState(..), TurnstileOutput(..), pushS, coinS, mondayS, regularPerson, distractedPerson, hastyPerson, tuesdayS, luckyPair, randomTurnS) where 

import State
--import Control.Monad.State
import Debug.Trace
import System.Random

data TurnstileState = Locked | Unlocked
    deriving (Show, Eq)
data TurnstileOutput = Tut | Thank | Open
    deriving (Show, Eq)
data TurnstileInput = Coin | Push
  deriving (Eq, Show)

pushS, coinS :: State TurnstileState TurnstileOutput
pushS = do
    s <- get 
    put Locked
    case s of 
        Locked -> return Tut
        Unlocked -> return Open

coinS = do
    s <- get 
    put Unlocked
    return Thank

turnS :: TurnstileInput -> State TurnstileState TurnstileOutput
turnS = _turn where
    _turn Coin = coinS
    _turn Push = pushS

getsThroughS :: TurnstileInput -> State TurnstileState Bool 
getsThroughS i = (Open == ) `fmap` turnS i


countOpens :: [TurnstileInput] -> State TurnstileState Int 
{-
countOpens is = 
    let f count input = fmap (\b -> if b then count + 1 else count) (getsThroughS input) 
    in foldM f 0 is
-}  
countOpens = foldM incIfOpens 0 where
  incIfOpens :: Int -> TurnstileInput -> State TurnstileState Int
  incIfOpens n i = do
    g <- getsThroughS i
    if g then return (n+1) else return n

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = sequence [coinS, pushS, pushS, coinS, pushS]

regularPerson, distractedPerson, hastyPerson :: State TurnstileState [TurnstileOutput]

regularPerson = mapM turnS [Coin, Push]
    
distractedPerson = mapM turnS [Coin]

hastyPerson = do
    r <- pushS
    if r == Tut then do 
        rest <- mapM turnS [Coin, Push]
        return (r : rest)
    else return [r]

tuesdayS :: State TurnstileState [TurnstileOutput]
tuesdayS = concat <$> sequence [regularPerson, hastyPerson, distractedPerson, hastyPerson]

luckyPair :: Bool -> State TurnstileState Bool
luckyPair who =
    let first = if who then regularPerson else distractedPerson
        action = first *> pushS
    in (Open == ) `fmap` action

testTurnstile :: State TurnstileState Bool
testTurnstile = do
  init <- get
  put Locked
  check1 <- pushS
  coinS 
  state1 <- get
  put Unlocked
  check2 <- pushS
  coinS
  state2 <- get
  put init
  return (check1 == Tut && state1 == Unlocked && 
    check2 == Open && state2 == Unlocked)

saveCoinsS :: [TurnstileInput] -> State TurnstileState Int
saveCoinsS is = snd <$>
    foldM 
        (\ (last, count) i ->
            if last == Thank && i == Coin 
                then return (last, count + 1)
                else (, count) <$> turnS i   
            )
        (Tut, 0)
        is



test :: [TurnstileInput] -> State TurnstileState Int
test = foldM foo 0 where
    foo :: Int -> TurnstileInput -> State TurnstileState Int
    foo acc a = do
        turnS a
        return (acc + 1)
         

randomInputS :: State StdGen TurnstileInput
randomInputS = do
  b <- getRandomS
  return $ if b then Coin else Push

randomTurnS :: State (StdGen, TurnstileState) TurnstileOutput
randomTurnS = do
    r <- processing fstL randomInputS
    processing sndL $ turnS r
    

    

