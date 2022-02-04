data Maybe' a = 
    Just' a
    | Nothing'
    deriving (Show)

instance Functor Maybe' where
    fmap fab (Just' a) = Just' (fab a)
    fmap fab Nothing' = Nothing'

instance Applicative Maybe' where
    pure a = Just' a
    Just' fab <*> Just' a = Just' (fab a)
    Just' fab <*> Nothing' = Nothing'
    Nothing' <*> Just' a = Nothing'
    Nothing' <*>  Nothing' = Nothing'

instance Monad Maybe' where
    (Just' a) >>= famb = famb a
    Nothing' >>= famb = Nothing'

instance MonadFail Maybe' where
    fail s = error s

data Pair a = Pair a a
    deriving (Show)

instance Functor Pair where
    fmap ((a, b) -> (c, d)) (Pair x y) = Pair m n
        where (m, n) = 