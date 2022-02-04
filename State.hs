{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module State (State(..), state, evalState, execState, put, get, foldM, sequenceUntil, getRandomS, processingFst, processingSnd, fstL, sndL, processing) where

import System.Random
import Control.Monad hiding (foldM)

import Debug.Trace

newtype State s a =
    State {
        runState :: s -> (a, s)
    }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
    fmap f (State p) = state $ \s0 -> let (a, s1) = p s0 in (f a, s1)


{-
fmap f (State p) = state $ \s0 -> let (a, s1) = p s0 in (f a, s1)
fmap = \ f (State p) -> state $ \s0 -> let (a, s1) = p s0 in (f a, s1)
fmap = \ f (State p) -> state $ \s0 -> (f (fst (p s0)), (snd (p s0)))
fmap = \ f (State p) -> State (\s0 -> (      f (fst (p s0))   ,    (snd (p s0))        )        )




fmap (g . f)
\ (State p) -> State (\s0 ->   ((g . f) (fst (p s0))  ,   (snd (p s0)))    )
\ (State p) -> State (\s0 -> (g (f (fst (p s0))), snd (p s0)))


fmap g . fmap f
(fmap g) . (fmap f)
\(State x) -> (fmap g) ((fmap f) (State x))
\(State x) -> (fmap g) (      (\(State p) -> State (\s0 -> (f (fst (p s0)), (snd (p s0)))))      (State x))
\(State x) -> (fmap g)         (State (\s0 -> (f (fst (x s0)), (snd (x s0))))) 
\(State x) -> State (\s0 -> (g (fst ((\s0 -> (f (fst (x s0)), (snd (x s0))))  s0)), (snd (x s0))))      
\(State x) -> State (\s0 -> (g (fst  (    (f (fst (x s0)), (snd (x s0))))           ) , (snd (x s0))      ))      
\(State x) -> State (\s0 -> (g (f (fst (x s0))), snd (x s0)))      

DONE!
-}

instance Applicative (State s) where
    pure a = state (a,)

    State pf <*> State px = state $ 
        \s0 -> 
            let
                (f, s1) = pf s0
                (x, s2) = px s1 
            in (f x, s2)
        

{-

    State pf <*> State px = state $ 
        \s0 -> 
            let
                (f, s1) = pf s0
                (x, s2) = px s1 
            in (f x, s2)
            


pure id <*> v = v
pure id <*> (State px) = (State px)

pure id <*> (State px) 
state (id, ) <*> (State px) 
State (id, ) <*> (State px) 

state $ 
        \s0 -> 
            let
                (f, s1) = (id, ) s0
                (x, s2) = px s1 
            in (f x, s2)

state $ 
        \s0 -> 
            let
                (f, s1) = (id, s0)
                (x, s2) = px s1 
            in (f x, s2)

state $ 
        \s0 -> 
            let
                s1 = s0
                (x, s2) = px s1 
            in (id x, s2)

state $ 
        \s0 -> 
            let
                s1 = s0
                (x, s2) = px s1 
            in (x, s2)

state $ 
        \s0 -> 
            px s0

state $ px

State px

DONE
        
-}


instance Monad (State s) where
    return x = state (x, )

    p >>= k = state $
        \s0 ->
            let
                (x, s1) = runState p s0      -- applies the initial state function to a passed-in state to get the new value & new state         
            in (runState . k) x s1

{-

m >>= return = m
(State mf) >>= return = (State mf)

return 
\x -> state (x,)
\x -> State (\y -> (x, y))

(State mf) >>= return 

state $
    \s0 ->
        let
            (x, s1) = runState (State mf) s0
        in (runState . return) x s1

state $
    \s0 ->
        let
            (x, s1) = mf s0
        in (runState . return) x s1

state $
    \s0 ->
        let
            (x, s1) = mf s0
        in (runState (return x)) s1

state $
    \s0 ->
        let
            (x, s1) = mf s0
        in (runState ((\x -> State (\y -> (x, y))) x)) s1

state $
    \s0 ->
        let
            (x, s1) = mf s0
        in (runState (State (\y -> (x, y)))) s1

state $
    \s0 ->
        let
            (x, s1) = mf s0
        in (\y -> (x, y)) s1

state $
    \s0 ->
        let
            (x, s1) = mf s0
        in (x, s1)

state $
    \s0 -> mf s0

state mf

State mf

-}


type StateFun a s = s -> (a, s)

compose :: StateFun a s -> (a -> StateFun b s) -> StateFun b s
compose f g s0 = let
    (a1, s1) = f s0
    (b2, s2) = g a1 s1
    in (b2, s2)

newtype Wrapped a = Wrapped a
wrap = Wrapped
unwrap (Wrapped a) = a


composeWrapped :: Wrapped (StateFun a s) -> (a -> Wrapped (StateFun b s)) -> Wrapped (StateFun b s)
composeWrapped wf g =
    wrap (\s0 ->
        let
            (a1, s1) = unwrap wf s0
            (b2, s2) = unwrap (g a1) s1
        in (b2, s2)
    )

composeWrapped' :: Wrapped (StateFun a s) -> (a -> Wrapped (StateFun b s)) -> Wrapped (StateFun b s)
composeWrapped' wf gw = wrap $ compose (unwrap wf) (unwrap . gw)

evalState :: State s a -> s -> a
evalState ss s = fst (runState ss s)

execState :: State s a -> s -> s
execState ss s = snd (runState ss s)

put :: s -> State s ()
put s = state $ const ((), s)

get :: State s s 
get = state $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = state (\s -> ((), f s))

gets :: (s -> a) -> State s a
gets f = state (\s -> (f s, s))

foldM :: (a -> b -> State s a) -> a -> [b] -> State s a
foldM f init = foldl foo (return init)  where
    foo ma b = ma >>= flip f b

addS :: State s [a] -> State s a -> State s [a]
addS x y = do
    as <- x     -- as :: [a]
    a <- y      -- a :: a
    let r = as ++ [a]
    return r

sequenceUntil :: (a -> Bool) -> [State s a] -> State s [a]
sequenceUntil p [] = return []
sequenceUntil p (ma :  mas) = do
    a <- ma
    if p a then return [a]
        else (a :) <$> sequenceUntil p mas

sequenceUntilM :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntilM p [] = return []
sequenceUntilM p (ma :  mas) = do
    a <- ma
    if p a then return [a]
        else do 
            rest <- sequenceUntilM p mas
            return (a : rest)
            
getRandomS :: Random a => State StdGen a
getRandomS = state random

processingFst :: State a o -> State (a,b) o
processingFst m = do
  (s1,s2) <- get
  let (o,s1') = runState m s1
  put (s1',s2)
  return o

processingSnd :: State b o -> State (a,b) o
processingSnd m = do
  (sa, sb) <- get
  let (o, sb') = runState m sb
  put (sa, sb')
  return o



data Lens cmb sub = Lens { 
    view :: cmb -> sub,
    set  :: cmb -> sub -> cmb
}

fstL :: Lens (a, b) a
fstL = Lens {
    view = fst,
    set = \(_, b) a' -> (a', b)
}

sndL :: Lens (a, b) b
sndL = Lens {
    view = snd,
    set = \(a, _) b' -> (a, b')
}

processing :: Lens cmb sub -> State sub o -> State cmb o
processing l m = do
    cmb <- get
    let (o, sub') = runState m (view l cmb)
    put (set l cmb sub')
    return o
