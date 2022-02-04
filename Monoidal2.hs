class Applicative f => Monoidal f where

unit :: Applicative f => f ()
unit = pure ()


(*&*) :: Applicative f => f a -> f b -> f (a, b)
ma *&* mb =  pure (,) <*> ma <*> mb

pure' :: Applicative f => a -> f a
pure' a = const a `fmap` unit

newApply :: Applicative f => f (a -> b) -> f a -> f b
newApply mf ma = uncurry ($) `fmap` (mf *&* ma)

{-

f <$> u <*> v = flip f <$> v <*> u


(f <$> u) <*> v = (flip f <$> v) <*> u

(f <$> u) *&* v = (flip f <$> v) *&* u




-}
