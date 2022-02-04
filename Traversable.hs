{-# OPTIONS_GHC -Wno-missing-methods #-}
deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x

rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = sequenceA . fmap deleteIfNegative

testList = [-5,3,2,-1,0]

rejectWithNegatives' :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives' = traverse deleteIfNegative

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Foldable Tree where
instance Functor Tree where 

instance Traversable Tree where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Branch ta tb) = 
        let
            mta = traverse f ta
            mtb = traverse f tb
        in
            Branch <$> mta <*> mtb

{-

traverse (\x -> [0..x])             [0..2]

(sequenceA . fmap (\x -> [0..x]))   [0..2]




-}

-- transpose [row1, row2, row3]
transpose :: [[a]] -> [[a]]
transpose rows = sequenceA rows

testMatrix = [[1, 2], [3, 4], [5, 6]]

{-
    1   2
    3   4
    5   6
-}