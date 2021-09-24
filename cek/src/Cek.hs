module Cek (Exp (..), Lambda (..), D (..), Kont (..), isFinal, evaluate) where

type Var = String

data Lambda = Var :=> Exp

data Exp
  = Ref Var
  | Lam Lambda
  | Exp :@ Exp

type Σ = (Exp, Env, Kont)

newtype D = Clo (Lambda, Env)

type Env = Var -> D

type Program = Exp

data Kont
  = Mt
  | Ar (Exp, Env, Kont)
  | Fn (Lambda, Env, Kont)

step :: Σ -> Σ
step (Ref x, p, κ) =
  (Lam lam, p', κ)
  where
    Clo (lam, p') = p x
step (f :@ e, p, κ) =
  (f, p, Ar (e, p, κ))
step (Lam lam, p, Ar (e, p', κ)) =
  (e, p', Fn (lam, p, κ))
step (Lam lam, p, Fn (x :=> e, p', κ)) =
  (e, p' // [x ==> Clo (lam, p)], κ)
step _ = error "should never happened"

terminal :: (Σ -> Σ) -> Σ -> Σ
terminal step ς0
  | isFinal ς0 = ς0
  | otherwise = terminal step (step ς0)

inject :: Program -> Σ
inject e = (e, p0, Mt)
  where
    p0 :: Env
    p0 = \x -> error $ "no binding for " ++ x

(==>) :: a -> b -> (a, b)
(==>) x y = (x, y)

(//) :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
(//) f [(x, y)] x' =
  if x == x'
    then y
    else f x'
(//) f _ x' = error "unknown case"

isFinal :: Σ -> Bool
isFinal (Lam _, p, Mt) = True
isFinal _ = False

evaluate :: Program -> Σ
evaluate pr = terminal step (inject pr)
