module Monoids where 

import Data.Bool

newtype And  = And { getAnd :: Bool }

instance Semigroup And where
  (<>) = mappend

instance Monoid And where
    mempty = And True
    And x `mappend` And y = And (x && y)

{-
    Laws
    
    (x <> y) <> z = x <> (y <> z)

    (And x <> And y) <> And z = And x <> (And y <> And z)
    And (x && y)     <> And z = And x <> And (y && z)
    And ((x && y) && z)       = And (x && (y && z))

    && is associative


    mempty <> x = x

    And True `mappend` (And x) = And x    
    And (True && x)            = And x
    And x                      = And x

    x <> mempty = x

    And x `mappend` (And True) = And x
    And (x && True)            = And x
    And x                      = And x
    
-}

xor :: Bool -> Bool -> Bool 
xor False False = False
xor False True = True 
xor True False = True 
xor True True = True

newtype Xor = Xor { getXor :: Bool }

instance Semigroup Xor where (<>) =  mappend

instance Monoid Xor where
    mempty = Xor False 
    mappend (Xor a) (Xor b) = Xor (a `xor` b)

{-

(x <> y) <> z = x <> (y <> z)

(Xor x <> Xor y) <> Xor z = Xor x <> (Xor y <> Xor z)
Xor (a `xor` b) <> Xor z  = Xor x <> Xor (y `xor` z)
Xor ((a `xor` b) `xor` z) = Xor (x `xor (y `xor` z))

FFF
FFT
FTF
FTT



-}