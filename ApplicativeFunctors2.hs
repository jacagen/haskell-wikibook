{-# OPTIONS_GHC -Wno-missing-methods #-}
newtype List a = List [a] deriving Show

instance Functor List where

instance Applicative List where
    List (f:fs) <*> List (a:as) = f a `colon` (List [f] <*> List as) `append` (List fs <*> List (a:as))
    _ <*> _                     = List []

colon :: a -> List a -> List a
colon a (List bs) = List (a:bs)

append :: List a -> List a -> List a
append (List as) (List bs) = List (as ++ bs)


class Applicative f => Backwards f where
    (<|*|>) :: f (a -> b) -> f a -> f b

instance Backwards List where
    List (f:fs) <|*|> List (a:as)   = f a `colon` (List fs <*> List [a]) `append` (List (f:fs) <*> List as)
    _ <|*|> _                       = List []


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

ZipList is commutative
((-> r)) is commutative 
State s is not commutative

-}

{-
[2,7,8] *> [3,9] = [6, 18, 21, 63, 24, 72]
-}

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
ma <**> mf = mf <*> ma


{-

[(*2), (*3), (*4)] <*> [5, 6]

[5, 6] >>= [(*2), (*3), (*4)]


-}