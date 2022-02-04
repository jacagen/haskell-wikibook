import System.Random
import Control.Monad.State ( MonadState(state), State, replicateM )
import Control.Applicative

rollPair :: StdGen -> ((Int, Int), StdGen)
rollPair sg = let
        (d1, sg1) = randomR (1, 6) sg
        (d2, sg2) = randomR (1, 6) sg1
    in ((d1, d2), sg2)

rollSix :: StdGen -> ([Int], StdGen)
rollSix sg = let
        (d1, sg1) = randomR (1, 6) sg
        (d2, sg2) = randomR (1, 6) sg1
        (d3, sg3) = randomR (1, 6) sg2
        (d4, sg4) = randomR (1, 6) sg3
        (d5, sg5) = randomR (1, 6) sg4
        (d6, sg6) = randomR (1, 6) sg5
    in ([d1, d2, d3, d4, d5, d6], sg6)

rollN :: Int -> StdGen -> ([Int], StdGen)
rollN n sg =
    let perform (_, sg') = randomR (1, 6) sg'
        s = iterate perform (randomR (1, 6) sg)
        rolls = take n s
    in (fst `map` rolls, snd $ last rolls)

rollDieS :: State StdGen Int
rollDieS = state $ randomR (1, 6)

rollNS :: Int -> State StdGen [Int]
--rollNS n = mapM (const rollDieS) [1..n]
rollNS n = replicateM n rollDieS 

rollPairS :: State StdGen (Int, Int)
{-
rollPairS = do
    r1 <- rollDieS
    r2 <- rollDieS
    return (r1, r2)
-}
rollPairS = liftA2 (,) rollDieS rollDieS

rollSixS :: State StdGen [Int]
rollSixS = do
    r1 <- rollDieS
    r2 <- rollDieS
    r3 <- rollDieS
    r4 <- rollDieS
    r5 <- rollDieS
    r6 <- rollDieS
    return [r1, r2, r3, r4, r5, r6]

luckyDoubleS :: State StdGen Int
luckyDoubleS = do
    r1 <- rollDieS
    if r1 == 6 then 
        (r1 +) `fmap` rollDieS 
    else
        return r1

rollDieDoubledS :: State StdGen Int
rollDieDoubledS = (* 2) `fmap` rollDieS

roll2Summed :: State StdGen Int
roll2Summed = liftA2 (+) rollDieS rollDieS

happyDoubleS :: State StdGen Int
{-
happyDoubleS = do
    r1 <- rollDieS
    r2 <- rollDieS
    return (if r1 == 6 then 2 * (r1 + r2) else r1 + r2)
-}
happyDoubleS = liftA2 
    (\ a b -> if a == 6 then 2 * (a + b) else a + b)
    rollDieS
    rollDieS

randomElt :: [a] -> State StdGen a
{-
randomElt as = do
    rg0 <- get
    let (r, rg1) = randomR (0, length as - 1) rg0
    put rg1
    return $ as !! r
-}
randomElt as = (as !!) <$> state (randomR (0, length as - 1))

