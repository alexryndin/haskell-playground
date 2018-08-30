import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
import Data.Char
import Control.Monad.State


data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

iotest :: State (Int, Int) Int
iotest = do
  (a,b) <- get
  return a


--nTree tree = fst $ numberTree 1 tree
--numberTree :: Integer ->  Tree () -> (Tree Integer, Integer)
--numberTree s (Leaf ()) = (Leaf s, s)
--numberTree s (Fork l x r) = let
--  (t1, s1)= numberTree s l
--  (t2, s2) = numberTree (s1+2) r
--  in (Fork (t1) (s1+1) (t2), s)


numberTree :: Tree () -> Tree Int
numberTree tree = evalState (nTree tree) 1

nTree :: Tree () ->  State Int (Tree Int)
nTree (Fork l x r) = do
  l' <- nTree l
  n <- get
  put $ n+1
  r' <- nTree r
  return $ Fork l' n r'
  

nTree (Leaf ()) = do
  n <- get
  put $ n+1
  return $ Leaf n
