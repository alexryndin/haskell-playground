import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
  fmap g (Leaf x) = Leaf (fmap g x)
  fmap g (Branch l x r) = Branch (fmap g l) (fmap g x) (fmap g r) 

-- class Functor f where
--  fmap :: (a -> b) -> f a -> f b


data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
  fmap f (Entry (k1, k2) v) = Entry (k1, k2) $ f v

instance Functor (Map k1 k2) where
  fmap f (Map x) = Map (map (fmap f) x)

data Log a = Log [String] a deriving Show

instance Functor Log where
  fmap f (Log [msg] x) = Log [msg] $ f x

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
  return x = Log [] x
  (>>=) (Log xs a) f = let 
    Log msg2 b = f a 
    in Log (msg2 ++ xs ++ msg2) b

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] $ f x

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = let 
  Log [m1] b = f x
  Log [m2] c = g b
    in Log [m1, m2] c 

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xs a) f = let 
  Log [msg2] b = f a 
                        in Log (xs ++ [msg2]) b
  
--execLoggersList :: a -> [a -> Log a] -> Log a
--execLoggersList a xs = foldl (>>=) (return a) xs 
