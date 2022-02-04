

import System.Random
import Control.Monad
import Control.Monad.Random.Strict
import Control.Monad.State
import Foreign (fillBytes)
import Language.Haskell.TH.PprLib (fcat)
import Lukko (fdClose)

class Functor' m where
    fmap' :: (a -> b) -> m a -> m b

data Tree a = Node a [Tree a]

instance Functor' Tree where
    fmap' f (Node a ts) = Node (f a) (fmap' f `map` ts)

instance Functor' (Either e) where
    fmap' _ (Left a) = Left a
    fmap' f (Right b) = Right (f b)


instance Functor' ((->) r) where
    fmap' = (.)


{-

Maybe

pure id <*> v = v                            -- Identity

If v is nothing
    Just id <*> Nothing = Nothing
    Nothing =  Nothing

If v is Just v'
    Just id <*> Just v' = Just v'
    Just (id v')        = Just v'
    Just v'             = Just v'

---

pure f <*> pure x   = pure (f x)               -- Homomorphism

Just f <*> Just x   = Just (f x)
Just (f x)          = Just (f x)

--

u <*> pure y = pure ($ y) <*> u              -- Interchange

u <*> Just y = Just ($ y) <*> u
    if u is Nothing
    Nothing <*> Just y  = Just ($ y) <*> Nothing
    Nothing             = Nothing

    if is Just u'
    Just u' <*> Just y  = Just ($ y) <*> Just u'
    Just (u' y)         = Just (($ y) u')
    Just (u' y)         = Just (u' $ y)
    Just (u' y)         = Just (u' y)

---

pure (.) <*> u <*> v <*> w      = u <*> (v <*> w) -- Composition
Just (.) <*> u <*> v <*> w      = u <*> (v <*> w)
    Nothing, Nothing, Nothing
    Just (.) <*> Nothing <*> Nothing <*> Nothing    = Nothing <*> (Nothing <*> Nothing)
    Nothing              <*> Nothing <*> 

    Nothing, Nothing, Just w
    Just (.) <*> Nothing <*> Nothing <*> Just w      = Nothing <*> (Nothing <*> Just w)
    Nothing              <*> Nothing <*> Just w      = Nothing <*> Nothing
    Nothing                          <*> Just w      = Nothing
    Nothing                                          = Nothing

    Nothing, Just v, Nothing
    Just (.) <*> Nothing <*> Just v <*> Nothing = Nothing <*> (Just v <*> Nothing)
    Nothing              <*> Just v <*> Nothing = Nothing <*> Nothing
    Nothing                         <*> Nothing = Nothing
    Nothing                                     = Nothing

    Nothing, Just v, Just w
    Just (.) <*> Nothing <*> Just v <*> Just w  = Nothing <*> (Just v <*> Just w)
    Just (.) <*> Nothing <*> Just v <*> Just w  = Nothing <*> (Just v <*> Just w)
    Nothing              <*> Just v <*> Just w  = Nothing <*> Just (v w)
    Nothing                                     = Nothing
    ...

    Just u, Just v, Just w
    Just (.) <*> Just u <*> Just v <*> Just w   = Just u <*> (Just v <*> Just w)
    Just (.) <*> Just u <*> Just v <*> Just w   = Just u <*> (Just v <*> Just w)
    Just (. u)          <*> Just v <*> Just w   = Just u <*> Just (v w)
    Just u              <*> Just v <*> Just w   = Just u <*> Just (v w)
    Just (u v)                     <*> Just w   = Just (u (v w))
    Just ((u v) w)                            =
    Just (u (v w))

-}

class (Functor' f) => Applicative' f where
    pure'  :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b

instance Applicative' (Either e) where
    pure' a = Right a

    Right f <**> Right a    = Right (f a)
    Left e <**> _           = Left e
    Right f <**> Left e     = Left e

instance Applicative' ((->) r) where 
    pure' = const
    (<**>) fab fa = \r -> fab r $ fa r

ap' :: Monad f => f (a -> b) -> f a -> f b
mf `ap'` ma = mf >>= (`fmap` ma)

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> k) -> f a -> f b -> f c -> f d -> f e -> f k
liftA5 x fa fb fc fd fe = 
    (x <$> fa)      -- f (b -> c -> d -> e -> k)
        <*> fb      -- f (c -> d -> e -> k)
        <*> fc
        <*> fd
        <*> fe


{- 
[(2*),(3*)]<*>[4,5]
[8, 10, 12, 15]

-}

--(<|*|>) :: Applicative f => f (a -> b) -> f a -> f b

(<|*|>) :: [a -> b] -> [a] -> [b]
(<|*|>) f a = [y x | x <- a, y <- f]

{- 

f <$> u <*> v = flip f <$> v <*> u

uvx <$> mu <*> mv = flip uvx <$> mv <*> mu

f :: u -> v -> x

do
    u <- mu
    let vx = uvx mu
    v <- mv
    return vx v

do
    let vux = flip uvx
    v <- mv
    let ux = vux v
    u <- mu
    return ux u

-}


{-

f <$> a <*> b <*> c <*> 

-}