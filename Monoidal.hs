{-# LANGUAGE ConstrainedClassMethods #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

class Applicative f => Monoidal f where
    unit :: Applicative f => f ()
    unit = pure ()

    (*&*) :: Applicative f => f a -> f b -> f (a, b)
    ma *&* mb =  pure (,) <*> ma <*> mb

newtype ZipList a = ZipList { getZipList :: [a] }
    deriving Show

instance Functor ZipList 

instance Applicative ZipList

instance Monoidal ZipList  where
    unit = ZipList $ repeat ()

    ZipList as *&* ZipList bs = ZipList (z as bs) where
        z :: [a] -> [b] -> [(a, b)]
        z (a:as) (b:bs) = (a, b) : z as bs
        z [] _          = []
        z _ []          = []

instance Monoidal ((->) r) where
    unit        = const ()
    fa *&* fb   = \x -> (fa x, fb x) 


