{-# OPTIONS_GHC -Wno-missing-methods #-}
import Control.Applicative
import Control.Monad.State
import Control.Monad
import Data.Tuple (swap)
import Data.Traversable

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x


newtype Checker a = Checker { checker :: Maybe [a] }

instance Semigroup (Checker a)

instance Monoid (Checker a) where
    mempty = Checker $ Just []
    mappend (Checker Nothing ) _ = Checker Nothing
    mappend _ (Checker Nothing) = Checker Nothing
    mappend (Checker (Just as)) (Checker (Just bs)) = Checker $ Just (as ++ bs)


rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives ns = checker $ foldMap (\x -> Checker ( (: []) `fmap` deleteIfNegative x)) ns


a = [   ["a1", "a2"],
        ["b1", "b2"],
        ["c1", "c2"]    ]

a' = [  ["a1", "b1", "c1"],
        ["a2", "b2", "c2"]  ]


transpose :: [[a]] -> [[a]]
transpose as = getZipList $ sequenceA $ ZipList <$> as

mapAccumL' :: Traversable t => (b -> a -> (c, a)) -> a -> t b -> (t c, a)
mapAccumL' step z t =
    let
        mstep = state . step
        trav = traverse mstep t
    in
        runState trav z


f b a = (b * a, b + a)
z = 10
bs = [2, 4, 6, 8]

result = mapAccumL' f z bs

{-

[20, 12]
[48, 16]
[96, 22]
[176, 30]
-}
