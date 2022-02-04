module AT (AT(..), AT'(..), fructify, reproduce) where 

import Control.Monad
import Control.Applicative


data AT a = L a | B (AT a) (AT a)
    deriving Show

instance Functor AT where 
    fmap f (L a) = L (f a)
    fmap f (B ma1 ma2) = B (fmap f ma1) (fmap f ma2)


instance Applicative AT where
    pure = L

    L f         <*> ma  = fmap f ma
    B mf1 mf2   <*> ma  = B (mf1 <*> ma) (mf2 <*> ma) 


instance Monad AT where
    return = L

    L a         >>= f   = f a
    B ma1 ma2   >>= f   = B (ma1 >>= f) (ma2 >>= f)

fructify :: AT a -> AT a
fructify ma = ma <**> B (L id) (L id)



reproduce :: (a -> b) -> (a -> b) -> AT a -> AT b
reproduce l r ma = B (L l) (L r) <*> ma

newtype AT' a = AT' (AT a)
    deriving Show

instance Functor AT' where 
    fmap f (AT' (L a))          = AT' $ L (f a)
    fmap f (AT' (B ma1 ma2))    = AT' $ B (fmap f ma1) (fmap f ma2) 

instance Applicative AT' where
    pure = AT' . L

    AT' mf <*> AT' ma = AT' (app mf ma) where
        app :: AT (a -> b) -> AT a -> AT b
        app (L f) (L a)             = L $ f a
        app lf@(L f) (B ma1 ma2)    = B (app lf ma1) (app lf ma2) 
        app (B mf1 mf2) (L a)       = undefined
        app (B mf1 mf2) (B ma1 ma2) = B (app mf1 ma1) (app mf2 ma2)


