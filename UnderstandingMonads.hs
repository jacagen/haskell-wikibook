{-# LANGUAGE ScopedTypeVariables #-}

import Data.Text.Encoding (Decoding(Some))
import Control.Monad (join)

{-
return :: a -> m a
(>>=)  :: m a -> (a -> m b) -> m b
-}

data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
    fmap f (Just' a) = Just' (f a)
    fmap _ Nothing' = Nothing'

instance Applicative Maybe' where
    pure = Just'
    Just' a <*> Just' b = Just' (a b)
    _ <*> _ = Nothing'

instance Monad Maybe' where
    return = Just'
    Just' a >>= f = f a
    Nothing' >>= f = Nothing'

data Person

father :: Person -> Maybe Person
father = error "Need to implement"

mother :: Person -> Maybe Person
mother = error "Need to implement"

maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p = mother p >>= father

bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p = 
    let 
        mg = mother p >>= father
        pg = father p >>= father
    in
        (,) <$> mg <*> pg


-- Bind in terms of join
altBind  :: Monad m => m a -> (a -> m b) -> m b 
ma `altBind` famb = join $ fmap famb ma 


-- fmap in terms of (>>=) & return
fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' fab ma = ma >>= (return . fab)

-- join in terms of (>>=) & return
join' :: Monad m => m (m a) -> m a
join' mma = mma >>= id

themselvesTimes :: [Int] -> [Int]
themselvesTimes = concat . fmap (\n -> replicate n n)