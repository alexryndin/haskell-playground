import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
import Data.Char
import Control.Monad.State

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

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Monad Log where
  return x = Log [] x
  (>>=) (Log xs a) f = let
    Log msg2 b = f a
    in Log (msg2 ++ xs ++ msg2) b

instance Monad Identity where
  return x = Identity x
  Identity x >>= k = k x

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Functor Identity where
  fmap _ _ = undefined

wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)

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


type Name = String
type DataBase = [(Name, Name)]

fathers, mothers :: DataBase
fathers = [("Bill", "John"),
          ("Ann","John"),
          ("John","Piter")]

mothers = [("Bill", "Jane"),
          ("Ann", "Jane"),
          ("John", "Alice"),
          ("Jane","Dorothy"),
          ("Alice", "Mary")]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken s = if all isDigit s then Just $ Number (read s :: Int) else Nothing
asToken _   = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = foldr f (return []) $ map asToken $ words input
  where
    f m ms = m >>= \x -> ms >>= \xs -> return (x:xs)
      --m >>= \x -> xs <- ms; return (x:xs)
      --do {x <- m; xs <- ms; return (x:xs)}

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
  a <- [1..x]
  b <- [1..x]
  c <- [1..x]
  True <- return (a < b)
  True <- return (a^2 + b^2 == c^2)
  return (a,b,c)

main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine
    if name /= "" then (putStrLn $ "Hi, " ++ name ++ "!") else main'


data Tree2 a = Leaf2 a | Fork (Tree2 a) a (Tree2 a) deriving Show

iotest :: State (Int, Int) Int
iotest = do
  (a,b) <- get
  return a

--numberTree :: Tree2 () -> Tree2 Integer
numberTree s (Leaf2 ()) = Leaf2 s
numberTree s (Fork (l x r)) = let
  s1 t1 = numberTree s l
  s2 t2 = numberTree (s1+2) r
  in Fork (Tree2 t1) (s1+1) (Tree2 t2)
