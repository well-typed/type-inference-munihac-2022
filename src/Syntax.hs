module Syntax where

type Name = String

data Expr =
    Var Name           -- x
  | App Expr Expr      -- e1 e2
  | Lam Name Expr      -- \ x -> e
  | Let Name Expr Expr -- let x = e1 in e2
  deriving Show

-- | Smart constructor for a chain of lambda abstractions.
lam :: [Name] -> Expr -> Expr
lam xs e = foldr Lam e xs

-- | Smart constructor for a let function binding.
letfun :: Name -> [Name] -> Expr -> Expr -> Expr
letfun x xs e1 e2 = Let x (lam xs e1) e2
