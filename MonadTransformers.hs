{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
import Data.Char
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.State (MonadState (..))
import Distribution.Simple.Utils (setFileOrdinary)

{-
getPassphrase :: IO (Maybe String)
getPassphrase = do 
    s <- getLine
    if isValid s then return $ Just s
        else return Nothing
-}
getPassphrase :: MaybeT IO String
getPassphrase = do
    s <- lift getLine
    guard $ isValid s
    return s

isValid :: String -> Bool 
isValid s = length s >= 8
    && any isAlpha s
    && any isNumber s
    && any isPunctuation s

askPassphrase :: MaybeT IO ()
askPassphrase = do
    lift $ putStrLn "Insert your new passphrase: "
    p <- msum $ repeat getPassphrase
    lift $ putStrLn "Storing your new password"

newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

instance Monad m => Functor (MaybeT m) where
    fmap = liftM

instance Monad m => Applicative (MaybeT m) where
    pure = return 
    (<*>) = ap

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    (>>=) (MaybeT a) f = MaybeT $ do    
        mba <- a
        case mba of 
            Just x -> runMaybeT $ f x
            Nothing -> return Nothing


instance Monad m => Alternative (MaybeT m) where
    empty = MaybeT $ return Nothing
    (<|>) (MaybeT f1) (MaybeT f2) = MaybeT $ 
        do
            m1 <- f1
            case m1 of
                Just x -> f1
                Nothing -> f2

instance Monad m => MonadPlus (MaybeT m) where
    mzero = empty 
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

instance Functor Identity where
    fmap = liftM

instance Applicative Identity where
    pure = return 
    (<*>) = ap

newtype IdentityT m a = IdentityT (m a)

instance Monad m => Functor (IdentityT m) where
    fmap = liftM

instance Monad m => Applicative (IdentityT m) where
    pure = return 
    (<*>) = ap

instance Monad m => Monad (IdentityT m) where
    return  = IdentityT . return
    (>>=) (IdentityT ma) f = IdentityT $ do
        a <- ma
        let (IdentityT x) = f a
        x

instance MonadTrans IdentityT  where
  lift = IdentityT

newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }


instance Monad m => Functor (StateT s m) where
    fmap = liftM

instance Monad m => Applicative (StateT s m) where
    pure = return 
    (<*>) = ap


instance Monad m => Monad (StateT s m) where
    (>>=) (StateT sf) f = StateT $ \s0 -> do
        (a1, s1) <- sf s0
        let fa1 = runStateT (f a1)
        fa1 s1

instance (Monad m) => MonadState s (StateT s m) where
    get = StateT $ \s -> do return (s, s)
    put s = StateT $ \_ -> do return ((), s)

instance Monad m => Alternative (StateT s m) where
    empty = undefined
    (<|>) = undefined

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero = StateT $ \s -> ( , s) `fmap` mzero
    mplus (StateT fx) (StateT fy) = StateT $ \s0 -> mplus (fx s0) (fy s0)

instance MonadTrans (StateT s) where  
    lift ma = StateT $ \s -> (, s) `fmap` ma

state :: MonadState s m => (s -> (a, s)) -> m a
state sf = do
    s <- get 
    let (a1, s1) = sf s
    put s1
    return a1

fn :: Int -> Maybe [Int]
fn idx = do let l = [Just [1,2,3], Nothing, Just [], Just [7..20]]
            (x:xs) <- l!!idx   -- a pattern match failure will call "fail"
            return xs