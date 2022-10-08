{-# LANGUAGE InstanceSigs #-}
module Types where

import Data.List
import Data.Map

import Syntax

-- Syntax of types

-- | Monotypes
data MType =
    TVar Name        -- ^ inference (or quantified) variable
  | Con Name         -- ^ type constants (e.g. Int, Bool)
  | Arr MType MType  -- ^ function type
    -- optional parameterised type
  | List MType       -- ^ list type
  deriving (Show)

example1 :: MType
example1 = intTy ~> intTy -- Arr (Con "Int") (Con "Int")  -- Int -> Int

example2 :: MType
example2 = Arr (TVar "a") (TVar "b")  -- a -> b

example3 :: PType
example3 = Forall ["a", "b"] example2   -- forall a b. a -> b



-- | Polytypes (aka type schemes)
data PType =
    Forall [Name] MType
  deriving (Show)

type Env = Map Name PType

intTy :: MType
intTy = Con "Int"

boolTy :: MType
boolTy = Con "Bool"

listTy :: MType -> MType
listTy = List

(~>) :: MType -> MType -> MType
(~>) = Arr

infixr 7 ~>
infixr `Arr`

-- Pretty-printing for types

class Pretty a where
  pretty :: a -> String

instance Pretty PType where
  pretty :: PType -> String
  pretty (Forall [] t) =
    pretty t
  pretty (Forall xs t) =
    "forall " ++ intercalate " " xs ++ ". " ++ pretty t

instance Pretty MType where
  pretty :: MType -> String
  pretty = go False
    where
      go :: Bool -> MType -> String
      go _ (TVar  i)   = i
      go _ (Con n)     = n
      go p (Arr t1 t2) = let b = go True t1 ++ " -> " ++ go False t2
                         in  if p then "(" ++ b ++ ")" else b
      -- optional
      go _ (List t)    = "[" ++ go False t ++ "]"
