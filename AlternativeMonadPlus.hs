{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char
import Control.Applicative
import Control.Monad
import Data.Foldable

{-
instance Alternative Maybe where
  empty               = Nothing
  -- Note that this could have been written more compactly.
  Nothing <|> Nothing = Nothing -- 0 results + 0 results = 0 results
  Just x  <|> Nothing = Just x  -- 1 result  + 0 results = 1 result
  Nothing <|> Just x  = Just x  -- 0 results + 1 result  = 1 result
  Just x  <|> Just y  = Just x  -- 1 result  + 1 result  = 1 result:
                                -- Maybe can only hold up to one result,
                                -- so we discard the second one.
-}

{-

Maybe:

empty <|> u = u
empty <|> u
Nothing <|> u
u

u <|> empty = u
u <|> empty
u <|> Nothing
u

u <|> (v <|> w) = (u <|> v) <|> w

If anything is Nothing, then the whole thing evaluates to nothing

u <|> (v <|> w)
Just u <|> (Just v <|> Just w)
Just u <|> Just v
Just u

(u <|> v) <|> w
(Just u <|> Just v) <|> Just w
Just u <|> Just w
Just u


-}

{-

instance Alternative [] where
  empty = []
  (<|>) = (++) -- length xs + length ys = length (xs ++ ys)

empty <|> u = u
[] <|> u 
[] ++ u
u

-}

digit i (c:_)
  | i > 9 || i < 0 = Nothing
  | otherwise      =
        if [c] == show i then Just i else Nothing

char :: Char -> String -> Maybe (Char, String)
char c s = do
  c' : s' <- return s
  guard (c == c')
  return (c, s')

apply :: b -> (b -> c) -> c
apply b f = f b

anyDigit :: String -> Maybe Int
anyDigit s = asum (map (apply s . digit) [0..9])

lowerHexChar :: String -> Maybe Int
lowerHexChar (c:_) = do
    guard $ c >= 'a'
    guard $ c <= 'f'
    return $ ord c - ord 'a' + 10

upperHexChar :: String -> Maybe Int
upperHexChar (c:_) = do
    guard $ c >= 'A'
    guard $ c <= 'F'
    return $ ord c - ord 'A' + 10

hex :: String -> Maybe Int
hex s = do
    upperHexChar s 
        <|> lowerHexChar s
        <|> anyDigit s

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog i = do 
    guard $ i > 0
    pure $ log i
