{-# OPTIONS_GHC -Wno-missing-methods #-}

import Data.Foldable (Foldable(toList))
import Data.Functor (($>))
import qualified Debug.Trace as Main
import Control.Applicative

foldMap' :: Monoid m => (a -> m) -> [a] -> m
foldMap' f = foldr (\a m -> f a <> m) mempty

foldMap'' :: Monoid m => (a -> m) -> [a] -> m
foldMap'' _ []      = mempty
foldMap'' f (a:as)  = f a <> foldMap'' f as

newtype Sum a = Sum { sum :: a }
    deriving Show

instance Semigroup (Sum a)
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum $ x + y


newtype Product a = Product { prod :: a }
    deriving Show

instance Semigroup (Product a)
instance Num a =>  Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product $ x * y

product :: (Foldable t, Num a) => t a -> a
product = prod . foldMap Product

newtype Concat a = Concat { ct :: [a] }

instance Semigroup (Concat a)
instance Monoid (Concat a) where
    mempty = Concat []
    Concat xs `mappend` Concat ys = Concat $ xs ++ ys

concat :: Foldable t => t [a] -> [a]
concat = ct . foldMap Concat

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f ta =
    let prep = f `fmap` toList ta
    in Main.concat prep

newtype And = And { ab :: Bool }
instance Semigroup And
instance Monoid And where
    mempty = And True
    And x `mappend` And y = And $ x && y

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p as =
    let prep = p `map` toList as
    in (ab . foldMap And) prep

newtype Any = Any { any :: Bool }
instance Semigroup Any
instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any $ x || y

elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem a as =
    let prep = (a ==) `map` toList as
    in (Main.any . foldMap Any) prep

length :: Foldable t => t a -> Int
length as = Main.sum $ foldMap (Sum . const 1) as


newtype Traverse f = Traverse { traverse :: f () }
instance Semigroup (Traverse f)
instance Applicative f => Monoid (Traverse f) where
    mempty = Traverse $ pure ()
    Traverse f1 `mappend` Traverse f2 = Traverse $ f1 *> f2 $> ()

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ fn as = Main.traverse $ foldMap (\a -> Traverse (fn a $> ())) as

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f as = Main.traverse  $ foldMap (\a -> Traverse (f a $> ())) as

newtype Max o = Max { max :: Maybe o } deriving Show
instance Semigroup (Max o)
instance Ord o => Monoid (Max o) where
    mempty = Max Nothing
    Max m1 `mappend` Max m2 = Max $ max' m1 m2
        where
            max' :: Ord o => Maybe o -> Maybe o -> Maybe o
            max' Nothing a  = a
            max' a Nothing  = a
            max' a b = Prelude.max a b


safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum as  = Main.max $ foldMap (Max . Just) as

newtype Find a = Find { unFind :: Maybe a }
instance Semigroup (Find a)
instance Monoid (Find a) where
    mempty = Find Nothing
    left@(Find (Just a)) `mappend` Find Nothing     = left
    Find Nothing `mappend` right@(Find (Just b))    = right
    ma `mappend` _                                  = ma

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p as =
    unFind $ foldMap (\x -> if p x then Find (Just x) else Find Nothing) as

newtype Left b = Left { unLeft :: b }
instance Semigroup (Left b)
instance Monoid (Left b) where
    mempty = undefined



newtype Compose a = Compose { unCompose :: a -> a }
instance Semigroup (Compose a)
instance Monoid (Compose a) where
    mempty = Compose id
    mappend (Compose a) (Compose b) = Compose $ b . a

composeL :: Foldable t => (b -> a -> b) -> t a -> b -> b
composeL f as b =
    let
        f' a = Compose $ flip f a
    in
        (unCompose (foldMap f' as)) b

hardFoldl2 f a es =
    (foldr (flip (.)) id (map (flip f) es)) a