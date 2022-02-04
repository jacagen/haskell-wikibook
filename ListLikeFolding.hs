data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving Show

instance Foldable Tree where
    foldr f b (Leaf a) = f a b 
    foldr f b (Branch t1 t2) = 
        let
            b1 = foldr f b t1
            b2 = foldr f b1 t2
        in
            b2


-- treeDepth not possible with Foldable

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold _ fa (Leaf a) = fa a
treeFold fb fa (Branch t1 t2) = fb (treeFold fb fa t1) (treeFold fb fa t2)

treeDepth :: Tree a -> Int
treeDepth = treeFold (\b1 b2 -> 1 + (max b1 b2)) (const 0)


testTree = 
    Branch 
        (Branch
            (Leaf 'a')
            (Branch 
                (Leaf 'b')
                (Branch
                    (Leaf 'c')
                    (Leaf 'd')
                )
            )
        )
        (Leaf 'd')


class Monoid' m where 
  mempty'  :: m
  mappend' :: m -> m -> m  -- Synonym: (<>)

instance (Monoid' a, Monoid' b) => Monoid' (a,b) where
    mempty' = (mempty', mempty')
    mappend' (a, b) (c, d) = (mappend' a c, mappend' b d)

{-

f mempty          = mempty
f (x `mappend` y) = f x `mappend` f y

fst :: (a, b) -> a

1)

fst (mempty, mempty) = mempty
mempty = mempty

2)

fst ((a, b) `mappend` (c, d))   = fst (a, b) `mappend` fst (c, d)
fst (mappend a c, mappend b d)  = a `mappend` c 
mappend a c                     = mappend a c


f &&& g = \x -> (f x, g x)
        = \x -> 

foldMap f &&& foldMap g             = foldMap (f &&& g)
                                    = foldMap ((f &&&) )


-}

toList' :: Foldable t => t a -> [a]
toList' =  foldMap (: []) 

{-

Use the monoid homomorphism property of foldMap presented above to prove that

foldMap f &&& foldMap g = foldMap (f &&& g)

where





-}

f &&& g = \x -> (f x, g x)

{-

foldMap f &&& foldMap g = foldMap (f &&& g)

\x -> ((foldMap f) x, (foldMap g) x) = foldMap (\x -> (f x, g x))

fst (\x -> ((foldMap f) x, (foldMap g) x))  = fst (foldMap (\x -> (f x, g x)))
                                            = fst . foldMap (\x -> (f x, g x))
                                            = foldMap (fst . \x -> (f x, g x))

-}