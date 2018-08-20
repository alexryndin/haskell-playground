data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add (Suc a) (Suc b) = add a $ Suc (Suc b) 
add Zero a = a
add a Zero = a

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul (Suc Zero) a = a
mul a (Suc Zero) = a
mul (Suc a) b = add b $ mul a b

fac :: Nat -> Nat
fac Zero = Suc Zero
fac (Suc Zero) = Suc Zero
fac a@(Suc a') = mul a $ fac a'

infixl 6 :+:
infixl 7 :*:
data Expr = X Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand (e1 :*: e) :+: expand (e2 :*: e)
expand (e :*: (e1 :+: e2)) = expand (e :*: e1) :+: expand (e :*: e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e

expandList :: Expr -> [Expr]
expandList (X i)   = [X i]
expandList (l :+: r) = expandList l ++ expandList r
expandList (l :*: r) = [ e1 :*: e2 | e1 <- expandList l, e2 <- expandList r]
