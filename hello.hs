import Data.Typeable
import Data.Char
import Data.List

main = putStrLn "Hello, world!"
-- discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

-- standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

fac 0 = 1
fac n = n * fac (n-1)
doubleFact :: Integer -> Integer
doubleFact n
  | n < 1 = 1
  | otherwise = n * doubleFact (n - 2)

fibonacci :: Integer -> Integer
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
  | n < 0  = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci' n = h 1 0 n
    
h p1 p2 n
  | n == 1 = p1
  | n == 0 = p2
  | n > 1 = h (p1+p2) p1 (n-1)
  | n < 0 = h (p2-p1) p1 (n+1)

seqA n
  | n >= 0 = let
      h' p1 p2 p3 2 = p1
      h' p1 p2 p3 1 = p2
      h' p1 p2 p3 0 = p3
      h' p1 p2 p3 n = h' (p1 + p2 - 2*p3) p1 p2 (n-1)
    in h' 3 2 1 n
  | otherwise = 3

getSecondFrom :: a -> a -> a2 -> a
getSecondFrom y x z = x


class Printable a where
  toString :: a -> String 

instance Printable Bool where
  toString True = "true" 
  toString False = "false"


instance (Printable a, Printable b) => Printable (a,b) where
  toString (a,b) = toString a ++ "," ++ toString b 

lucky :: Integer -> String
lucky 7 = "123"

mork x
  | x > 4^2 = 5

-- f :: Bounded a => a -> a
f a = maxBound


readDigits :: String -> (String, String)
readDigits [] = ([],[]) 
readDigits xs = span isDigit xs

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj a b xs = filter (\x -> a x || b x) xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:[]) = [x]
qsort xs@(x:xs') =
  let (a,b) = (filter (<x) xs' , filter (>=x) xs')
  in qsort a ++ [x] ++ qsort b

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

repeat2 x = x:repeat2 x

data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
  succ (Odd x) = Odd $ x + 2 
  pred (Odd x) = Odd $ x - 2
  enumFrom       x                  =     x : (enumFrom $ succ x)
  enumFromThen   (Odd x) (Odd next) = Odd x : (enumFromThen (Odd next) (Odd $ 2*next-x))
  enumFromTo     (Odd x) (Odd to)
    | x <= to     = Odd x : (enumFromTo (succ $ Odd x) (Odd to))
    | x >  to     = []
  enumFromThenTo (Odd x) (Odd next) (Odd to)
    | x < to && next < x   = []
    | x > to && next > x   = []
    | otherwise            = Odd x : (enumFromThenTo (Odd next) (Odd $ 2*next-x) (Odd to))

coins :: (Ord a, Num a) => [a]
coins = [7]
change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change s = [coin:ch | coin <- coins, s>0 , ch <- [[]]]

lengthList :: [a] -> Int
lengthList = foldr (const (+1)) 0 

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then s+x else 0) 0

fldltest :: (Num a) => [a] -> [a] 
fldltest = foldl' (\s x -> (x+1):s) []

evenOnly :: [a] -> [a]
evenOnly =  snd . foldr (\x ~(xs, ys) -> (x:ys, xs)) ([], [])

fold1 :: [a] -> a
fold1 ([x]) = x

fldl1 :: (a -> a -> a) -> [a] -> a
fldl1 _ [x] = x
fldl1 f (x:xs) = foldl f x xs
fldl1 _ [] = error "fldr isn work now"

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = (\(a,b) -> if b>=a then Just (b, (a,pred b)) else Nothing)

--perms :: [a] -> [[a]]
--perms [] = [[]]
--perms [x] = [[x]]
--perms x:xs = let xpnd x l = 
--in xpnd x xs

xpnd x [] = [[x]]
xpnd x l@(h:t) = (x:l) : map (h:) (xpnd x t)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (xpnd x) (perms xs)
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

data Color = Red | Green | Blue

foo True 4 = 7
data LogLevel = Error | Warning | Info

data Result' = Fail' Int | Success' deriving Show 

data Point = Double Double deriving Show

data Bit = Zero | One deriving (Ord, Eq)
data Sign = Minus | Plus 
data Z = Z Sign [Bit]

compareBits []     []  = EQ 
compareBits []     ys  = LT 
compareBits xs     []  = GT 
compareBits (xs) (ys) = case compare (length xs) (length ys) of
                          EQ -> compare (reverse xs) (reverse ys)
                          LT -> LT
                          GT -> GT

emptyZ = Z Plus []

add :: Z -> Z -> Z
add (Z Plus l1) (Z Plus l2) = Z Plus $ addBits Zero l1 l2
add (Z Plus l1) (Z Minus l2) = case compareBits l1 l2 of
                                 GT -> Z Plus $ subBits Zero l1 l2
                                 LT -> Z Minus $ subBits Zero l2 l1
                                 EQ -> Z Plus []

add (Z Minus l1) (Z Minus l2) = Z Minus $ addBits Zero l1 l2

add (Z Minus l1) (Z Plus l2)  = case compareBits l1 l2 of
                                  GT -> Z Minus $ subBits Zero l1 l2
                                  LT -> Z Plus  $ subBits Zero l2 l1
                                  EQ -> Z Plus [] 


-- addBits mem (x0:[]) (y0:y1:ys) = addBits mem (x0:Zero:[]) (y0:y1:ys)
-- addBits mem (x0:x1:xs) (y0:[]) = addBits mem (x0:x1:xs) (y0:Zero:[])

addBits Zero ([]) (ys) = ys
addBits Zero (xs) ([]) = xs
addBits One (xs) ([]) = addBits Zero xs (One:[])
addBits One ([]) (ys) = addBits Zero ys (One:[])


addBits mem (Zero:xs)(Zero:ys)  = mem : addBits Zero  (xs) (ys)
addBits Zero (One:xs)(Zero:ys)  = One : addBits Zero  (xs) (ys)
addBits Zero (Zero:xs)(One:ys)  = One : addBits Zero  (xs) (ys)
addBits Zero (One:xs)(One:ys)   = Zero : addBits One  (xs) (ys)
addBits One (One:xs)(Zero:ys)  = Zero : addBits One  (xs) (ys)
addBits One (Zero:xs)(One:ys)  = Zero : addBits One  (xs) (ys)
addBits One (One:xs)(One:ys)   = One : addBits One  (xs) (ys)


subBits Zero (xs) ([]) = xs
subBits One  ([xs]) ([]) = []
subBits One  (xs) ([]) = subBits Zero xs [One]
--subBits One (xs) ([]) = addBits Zero xs (One:[])
--subBits One ([]) (ys) = addBits Zero ys (One:[])

subBits Zero (Zero:xs)(Zero:ys)  = Zero : subBits Zero  (xs) (ys)
subBits Zero (One:xs)(Zero:ys)  = One : subBits Zero  (xs) (ys)
subBits Zero (Zero:xs)(One:ys)  = One : subBits One  (xs) (ys)
subBits Zero (One:xs)(One:ys)   = Zero : subBits Zero  (xs) (ys)
subBits One (One:xs)(Zero:ys)  = Zero : subBits Zero  (xs) (ys)
subBits One (Zero:xs)(One:ys)  = Zero : subBits One  (xs) (ys)
subBits One (One:xs)(One:ys)   = One : subBits One  (xs) (ys)
subBits One (Zero:xs)(Zero:ys)   = One : subBits One  (xs) (ys)

mul :: Z -> Z -> Z
mul (Z _ []) (Z _ _) = Z Plus [] 
mul (Z _ _) (Z _ []) = Z Plus [] 
mul (Z Plus l1) (Z Plus l2) = Z Plus $ mulBits l1 l2 l1 
mul (Z Plus l1) (Z Minus l2) = Z Minus $ mulBits l1 l2 l1 
mul (Z Minus l1) (Z Plus l2) = Z Minus $ mulBits l1 l2 l1 
mul (Z Minus l1) (Z Minus l2) = Z Plus $ mulBits l1 l2 l1 

mulBits xs [One] _ = xs
mulBits xs ys mlplr = mulBits (addBits Zero xs mlplr) (subBits Zero ys [One]) mlplr 


test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]


test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058
testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul

allTests = zip [0..] [test001, test002, test003, test011, test012, test013, test021, test022, test023, test031, test032, test033, test041, test042, test043, test051, test052, test053, test054, test055, test056, test057, test058, test101, test102, test103, test104, test105, test111, test112, test113, test114, test121, test122, test131]